package parser

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"log"
	"math"
	"reflect"
	"regexp"
	"strconv"
	"strings"

	"github.com/shopspring/decimal"
)

type PicType int

const (
	SIGNED_BINARY   PicType = iota // PIC S9(p) COMP, PIC S9 COMP
	UNSIGNED_BINARY                // PIC 9(p) COMP, PIC 9 COMP
	FLOAT4                         // PIC S9(p)V9(s) COMP-1
	FLOAT8                         // PIC S9(p)V9(s) COMP-2
	ALPHA_CHAR                     // PIC A(n)
	ANY_CHAR                       // PIC X(n)
	NUM_CHAR                       // PIC 9(n)
	DECIMAL                        // PIC S9(p)V9(s) COMP-3
	DISPLAY_NUMERIC
)

func (p PicType) String() string {
	switch p {
	case SIGNED_BINARY:
		return "SIGNED_BINARY"
	case UNSIGNED_BINARY:
		return "UNSIGNED_BINARY"
	case FLOAT4:
		return "FLOAT4"
	case FLOAT8:
		return "FLOAT8"
	case ALPHA_CHAR:
		return "ALPHA_CHAR"
	case ANY_CHAR:
		return "ANY_CHAR"
	case NUM_CHAR:
		return "NUM_CHAR"
	case DECIMAL:
		return "DECIMAL"
	case DISPLAY_NUMERIC:
		return "DISPLAY_NUMERIC"
	}
	return ""
}

type Field struct {
	label     string
	length    int32
	sLength   int32
	pLength   int32
	fieldType PicType
	startPos  int32
	Data      any
	rawData   []byte
}

type File struct {
	RecordLength int32
	Fields       []Field
	StartPos     int32
}

func getLength(fieldType PicType, newValue any) (int, error) {
	switch fieldType {
	case ANY_CHAR:
		stringVal, ok := newValue.(string)
		if !ok {
			return -1, errors.New("could not convert 'any' to string")
		}
		return len(stringVal), nil
	case ALPHA_CHAR:
		stringVal, ok := newValue.(string)
		if !ok {
			return -1, errors.New("could not convert 'any' to string")
		}
		return len(stringVal), nil
	case NUM_CHAR:
		numVal, ok := newValue.(string)
		if !ok {
			return -1, errors.New("could not convert 'any' to string")
		}
		return len(numVal), nil
	case DECIMAL:
		decimalVal, ok := newValue.(decimal.Decimal)
		if !ok {
			return -1, errors.New("could not convert 'any' to string")
		}
		decimalValLen := len(decimalVal.String())
		// Negatives have the - sign at the start, remove from length for consistency
		if decimalVal.IsNegative() {
			decimalValLen--
		}
		return decimalValLen, nil
	case UNSIGNED_BINARY:
		numVal := newValue.([]uint8)
		return len(numVal), nil
	case SIGNED_BINARY:
		numVal := newValue.([]int8)
		return len(numVal), nil
	case FLOAT4:
		floatVal := newValue.(float32)
		bits := math.Float32bits(floatVal)
		log.Printf("Bits: %08b", bits)
		return 4, nil
	default:
		return -1, fmt.Errorf("unsupported type for getLength %s", fieldType)
	}
}
func getLengthOfField(field *Field) (int, error) {
	switch field.fieldType {
	case ANY_CHAR:
		return int(field.length), nil
	case ALPHA_CHAR:
		return int(field.length), nil
	case NUM_CHAR:
		return int(field.length), nil
	case DECIMAL:
		return int(field.sLength + 1 + field.pLength), nil
	case UNSIGNED_BINARY:
		return int(field.length), nil
	case SIGNED_BINARY:
		return int(field.length), nil
	case FLOAT4:
		return int(3 + 1), nil
	default:
		return -1, fmt.Errorf("unsupported type for getLengthOfField %s", field.fieldType)
	}
}

func (f *Field) GetRawData() []byte {
	return f.rawData
}
func (f *Field) SetRawData(rawBytes []byte) {
	f.rawData = rawBytes
}

func (f *Field) SetData(newValue any) error {
	if reflect.TypeOf(newValue) != reflect.TypeOf(f.Data) {
		return fmt.Errorf("%v is of type %T, expected type %T", f.Data, newValue, f.Data)
	}
	length, err := getLength(f.fieldType, newValue)
	if err != nil {
		panic(err)
	}
	lengthOfField, err := getLengthOfField(f)
	if err != nil {
		panic(err)
	}
	if int32(length) > int32(lengthOfField) {
		log.Fatalf("length of %s %d greater than %d", f.label, length, lengthOfField)
	}
	log.Printf("Setting %s to %v", f.label, newValue)
	f.Data = newValue
	err = f.setFieldData(newValue)
	if err != nil {
		panic(err)
	}
	return nil
}

func (f *File) Field(fieldName string) (*Field, error) {
	for i, field := range f.Fields {
		if field.label == fieldName {
			log.Printf("Found field %s: i=%d, %s, startPos=%d, %s (%08b)", fieldName, i, field.label, field.startPos, field.Data, field.GetRawData())
			return &f.Fields[i], nil
		}
	}

	return nil, errors.New("no matching field label")
}

func (f *File) FieldNames() []string {
	var names []string
	for i, field := range f.Fields {
		log.Printf("Field Name: %d, %s, %d", i, field.label, field.startPos)
		names = append(names, field.label)
	}
	return names
}

func (f *File) Reparse(fieldName string) {

}

func (f *File) addField(field Field) {
	f.Fields = append(f.Fields, field)
	f.RecordLength += field.length + field.pLength + field.sLength
	f.StartPos = 0
}

func newFieldForString(label string, length int32, fieldType PicType) Field {
	f := Field{}
	f.label = label
	f.length = length
	f.fieldType = fieldType
	return f
}

func newFieldForDecimal(label string, p int32, s int32, fieldType PicType) Field {
	f := Field{}
	f.label = label
	f.sLength = s
	f.pLength = p
	f.fieldType = fieldType
	return f
}

func newFieldForSignedBinary(label string, length int32, fieldType PicType) Field {
	//https://techjogging.com/cobol-data-types.html
	f := Field{}
	f.label = label
	if length <= 4 {
		f.length = 2
	} else if length <= 9 {
		f.length = 4
	} else if length <= 20 {
		f.length = 8
	}
	f.fieldType = fieldType
	return f
}

func newFieldForUnsignedBinary(label string, length int32, fieldType PicType, startPos int32) Field {
	//https://techjogging.com/cobol-data-types.html
	f := Field{}
	f.label = label
	if length <= 4 {
		f.length = 2
	} else if length <= 9 {
		f.length = 4
	} else if length <= 20 {
		f.length = 8
	}
	f.fieldType = fieldType
	f.startPos = startPos
	return f
}

func newFieldForFloat(label string, s int32, p int32, fieldType PicType) Field {
	f := Field{}
	f.label = label
	f.sLength = s
	f.pLength = p
	f.fieldType = fieldType
	return f
}

func ParseLexData(lexer *Lexer) File {
	var file File

	stringREString := `^PIC X\((\d+)\)$`
	stringRE := regexp.MustCompile(stringREString)
	decimalREString := `^PIC S9\((\d+)\)V9\((\d+)\) COMP-3$`
	decimalRE := regexp.MustCompile(decimalREString)
	signedBinaryREString := `^PIC S9\((\d+)\) COMP$|^PIC S9 COMP$`
	signedBinaryRE := regexp.MustCompile(signedBinaryREString)
	unsignedBinaryREString := `^PIC 9\((\d+)\) COMP$|^PIC 9 COMP$`
	unsignedBinaryRE := regexp.MustCompile(unsignedBinaryREString)
	floatREString := `^PIC S9\((\d+)\)V9\((\d+)\) COMP-[12]$`
	floatRE := regexp.MustCompile(floatREString)
	alphaREString := `^PIC A\((\d+)\)$`
	alphaRE := regexp.MustCompile(alphaREString)
	numREString := `^PIC 9\((\d+)\)$`
	numRE := regexp.MustCompile(numREString)

	var lastIdent string
	var startPos int32 = 0
	for {
		pos, tok, lit := lexer.Lex()
		if tok == EOF {
			break
		}

		if tok == IDENT {
			lastIdent = lit
		}

		if tok == PIC {

			// Capture String Type: "PIC X(n)"
			capGroups := stringRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("%s\n", capGroups)
				foundLength, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				file.addField(newFieldForString(lastIdent, int32(foundLength), ANY_CHAR))
			}

			// Capture Decimal Type "PIC S9(p)V9(s) COMP-3"
			capGroups = decimalRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("%s\n", capGroups)
				foundPLength, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				foundSLength, err := strconv.Atoi(capGroups[2])
				if err != nil {
					panic(err)
				}
				log.Printf("p:%d, s:%d\n", foundPLength, foundSLength)

				file.addField(newFieldForDecimal(lastIdent, int32(foundPLength), int32(foundSLength), DECIMAL))
			}

			// Capture Signed Numeric Type "PIC S9(p) COMP" or "PIC S9 COMP"
			capGroups = signedBinaryRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("%s\n", capGroups)
				length := 1
				if len(capGroups) == 2 && capGroups[1] != "" {
					var err error
					length, err = strconv.Atoi(capGroups[1])
					if err != nil {
						panic(err)
					}

				}
				file.addField(newFieldForSignedBinary(lastIdent, int32(length), SIGNED_BINARY))
			}

			// Capture Numeric Unsigned Type "PIC 9(p) COMP" or "PIC 9 COMP"
			capGroups = unsignedBinaryRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("CapGroups: %s\n", capGroups)
				length := 1
				if len(capGroups) == 2 && capGroups[1] != "" {
					var err error
					length, err = strconv.Atoi(capGroups[1])
					if err != nil {
						panic(err)
					}

				}
				f := newFieldForUnsignedBinary(lastIdent, int32(length), UNSIGNED_BINARY, startPos)
				file.addField(f)
				startPos += f.length
			}

			// Capture Float Type "PIC S9(p)V9(s) COMP-1" or "PIC S9(p)V9(s) COMP-2"
			capGroups = floatRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("%s\n", capGroups)
				foundPLength, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				foundSLength, err := strconv.Atoi(capGroups[2])
				if err != nil {
					panic(err)
				}
				var floatType PicType
				if strings.Contains(lit, "COMP-1") {
					floatType = FLOAT4
				} else {
					floatType = FLOAT8
				}
				file.addField(newFieldForFloat(lastIdent, int32(foundPLength), int32(foundSLength), floatType))
			}

			// Capture Alpha Type "PIC A(n)"
			capGroups = alphaRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("%s\n", capGroups)
				length, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				file.addField(newFieldForString(lastIdent, int32(length), ALPHA_CHAR))
			}

			// Capture Num Type "PIC 9(n)"
			capGroups = numRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				log.Printf("%s\n", capGroups)
				length, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				file.addField(newFieldForString(lastIdent, int32(length), NUM_CHAR))
			}
		}

		log.Printf("%d:%d\t|%s|\t|%s|\n", pos.line, pos.column, tok, lit)
	}
	return file
}
func (field *Field) setFieldData(newValue any) error {
	m := map[string]uint8{
		"": 0, "â": 66, "ä": 67, "à": 68, "á": 69,
		"ã": 70, "å": 71, "ç": 72, "ñ": 73, "¢": 74,
		".": 75, "<": 76, "(": 77, "+": 78, "|": 79,
		"&": 80, "é": 81, "ê": 82, "ë": 83, "è": 84,
		"í": 85, "î": 86, "ï": 87, "ì": 88, "ß": 89,
		"!": 90, "$": 91, "*": 92, ")": 93, ";": 94,
		"¬": 95, "-": 96, "/": 97, "Â": 98, "Ä": 99,
		"À": 100, "Á": 101, "Ã": 102, "Å": 103, "Ç": 104,
		"Ñ": 105, "¦": 106, ",": 107, "%": 108, "_": 109,
		">": 110, "?": 111, "ø": 112, "É": 113, "Ê": 114,
		"Ë": 115, "È": 116, "Í": 117, "Î": 118, "Ï": 119,
		"Ì": 120, "`": 121, ":": 122, "#": 123, "@": 124,
		"'": 125, "=": 126, "\"": 127, "Ø": 128, "a": 129,
		"b": 130, "c": 131, "d": 132, "e": 133, "f": 134,
		"g": 135, "h": 136, "i": 137, "«": 138, "»": 139,
		"ð": 140, "ý": 141, "þ": 142, "±": 143, "°": 144,
		"j": 145, "k": 146, "l": 147, "m": 148, "n": 149,
		"o": 150, "p": 151, "q": 152, "r": 153, "ª": 154,
		"º": 155, "æ": 156, "¸": 157, "Æ": 158, "¤": 159,
		"µ": 160, "~": 161, "s": 162, "t": 163, "u": 164,
		"v": 165, "w": 166, "x": 167, "y": 168, "z": 169,
		"¡": 170, "¿": 171, "Ð": 172, "Ý": 173, "Þ": 174,
		"®": 175, "^": 176, "£": 177, "¥": 178, "·": 179,
		"©": 180, "§": 181, "¶": 182, "¼": 183, "½": 184,
		"¾": 185, "[": 186, "]": 187, "¯": 188, "¨": 189,
		"´": 190, "×": 191, "{": 192, "A": 193, "B": 194,
		"C": 195, "D": 196, "E": 197, "F": 198, "G": 199,
		"H": 200, "I": 201, "­": 202, "ô": 203, "ö": 204,
		"ò": 205, "ó": 206, "õ": 207, "}": 208, "J": 209,
		"K": 210, "L": 211, "M": 212, "N": 213, "O": 214,
		"P": 215, "Q": 216, "R": 217, "¹": 218, "û": 219,
		"ü": 220, "ù": 221, "ú": 222, "ÿ": 223, "\\": 224,
		"÷": 225, "S": 226, "T": 227, "U": 228, "V": 229,
		"W": 230, "X": 231, "Y": 232, "Z": 233, "²": 234,
		"Ô": 235, "Ö": 236, "Ò": 237, "Ó": 238, "Õ": 239,
		"0": 240, "1": 241, "2": 242, "3": 243, "4": 244,
		"5": 245, "6": 246, "7": 247, "8": 248, "9": 249,
		"³": 250, "Û": 251, "Ü": 252, "Ù": 253, "Ú": 254,
	}

	switch field.fieldType {
	case ALPHA_CHAR:
		stringVal, ok := newValue.(string)
		if !ok {
			return fmt.Errorf("unable to cast %v of type %T as string", newValue, newValue)
		}
		for i, v := range stringVal {
			newByte := m[string(v)]
			log.Printf("rune: %s -> %d", string(v), newByte)
			field.rawData[i] = newByte
		}
		return nil
	case ANY_CHAR:
		stringVal, ok := newValue.(string)
		if !ok {
			return fmt.Errorf("unable to cast %v of type %T as string", newValue, newValue)
		}
		for i, v := range stringVal {
			newByte := m[string(v)]
			log.Printf("rune: %s -> %d", string(v), newByte)
			field.rawData[i] = newByte
		}
		return nil
	case NUM_CHAR:
		stringVal, ok := newValue.(string)
		if !ok {
			return fmt.Errorf("unable to cast %v of type %T as string", newValue, newValue)
		}
		for i, v := range stringVal {
			newByte := m[string(v)]
			log.Printf("rune: %s -> %d", string(v), newByte)
			field.rawData[i] = newByte
		}
		return nil
	case DECIMAL:
		decimalVal, ok := newValue.(decimal.Decimal)
		if !ok {
			return fmt.Errorf("unable to cast %v of type %T as bigdecimal", newValue, newValue)
		}
		isNegative := decimalVal.IsNegative()
		if isNegative {
			decimalVal = decimalVal.Mul(decimal.NewFromInt(-1))
		}
		bytes := make([]byte, field.pLength+field.sLength)
		var decimalParts = strings.Split(decimalVal.String(), ".")
		leftSide := decimalParts[0]
		rightSide := decimalParts[1]
		for i := range leftSide {
			digit, err := strconv.Atoi(fmt.Sprintf("%d", leftSide[i]-48))
			if err != nil {
				panic(err)
			}
			/*
				Given pLength of 11, sLength of 2 we have
				> 00000000000.00
				The starting index of 123.45 is:
						V
				00000000000.00
				And we need pLength to be 0 based like array
				so take (pLength-1) - length(123)
			*/
			fieldIndex := (int(field.pLength)) - len(leftSide) + i
			log.Printf("Setting: %d = %v", fieldIndex, digit)
			bytes[fieldIndex] = byte(digit)
		}
		for i := range rightSide {
			digit, err := strconv.Atoi(fmt.Sprintf("%d", rightSide[i]-48))
			if err != nil {
				panic(err)
			}

			fieldIndex := int(field.pLength) + i
			log.Printf("Setting: %d = %v", fieldIndex, digit)
			bytes[fieldIndex] = byte(digit)
		}

		log.Printf("Raw data: %08b, isNegative: %v\n", bytes, isNegative)
		bytes = compress(int(field.pLength), int(field.sLength), bytes, isNegative)
		log.Printf("Final data: %08b", bytes)
		field.SetRawData(bytes)
		return nil
	case UNSIGNED_BINARY:
		log.Printf("Existing Raw Data: %08b", field.rawData)
		val := newValue.([]uint8)
		log.Printf("Unsigned Integer value: %08b", val)
		bufBytes := new(bytes.Buffer)
		binary.Write(bufBytes, binary.LittleEndian, val)
		log.Printf("Buffer Bytes: %08b", bufBytes.Bytes())
		field.SetRawData(bufBytes.Bytes())
		log.Printf("New Value: %08b", field.rawData)

		return nil
	case SIGNED_BINARY:
		log.Printf("Existing Raw Data: %08b", field.rawData)
		val := newValue.([]int8)
		if len(val) < int(field.length) {
			newVal := make([]int8, field.length)
			for i := range val {
				newVal[int(field.length)-1-i] = val[i]
			}
			val = newVal
		}
		log.Printf("Unsigned Integer value: %08b", val)
		bufBytes := new(bytes.Buffer)
		binary.Write(bufBytes, binary.LittleEndian, val)
		log.Printf("Buffer Bytes: %08b", bufBytes.Bytes())
		field.SetRawData(bufBytes.Bytes())
		log.Printf("New Value: %08b", field.rawData)

		return nil
	case FLOAT4:
		log.Printf("Existing Raw Data: %08b", field.rawData)
		uint32Bits := make([]byte, 4)
		//floatBits := math.Float32bits(newValue.(float32))
		float32Val := newValue.(float32)
		binary.LittleEndian.PutUint32(uint32Bits[:], math.Float32bits(float32Val))

		log.Printf("Float bits: %08b", uint32Bits)

		field.SetRawData(uint32Bits)
		log.Printf("New Value: %08b", field.rawData)
		return nil

	}
	return fmt.Errorf("unsupported mapping for field data %s", field.fieldType)
}

func compress(pLength int, sLength int, bytes []byte, isNegative bool) []byte {
	/*
		Binary Compression, each 8 bytes holds 2 4 byte digits from 0-9
		pLength is the digits to the left of the decimal
		sLength is the digits to the right of the decimal

		The sign is encoded in the bottom half of the last byte (13 is negative)

		Samples:
		   p    s     Total Bytes   Example
		   0    0     1             +/- 0         0000 ssss
		   0    1     1             +/- 0.9       1001 ssss
		   1    0     1             +/- 9         1001 ssss
		   1    1     2             +/- 9.9       1001 1001 0000 ssss
		   1    2     2             +/- 9.99      1001 1001 1001 ssss
		   2    1     2             +/- 99.9      1001 1001 1001 ssss
		   2    2     3             +/- 99.99     1001 1001 1001 1001 0000 ssss
		   3    1     3             +/- 999.9     1001 1001 1001 1001 0000 ssss


		Total target bytes = ((p + s) % 2) + 1

		If pLength is odd
			we have (pLength - 1) / 2 full bytes for the left side,
			plus half a byte with the lower half part of right side of decimal
			Dec: 123
			Len: 3
			Bin: 0001 0020, 0011 ????
				where ???? is the s part

		If pLength is even
			we have pLength / 2 in the full bytes for the left side
	*/
	log.Printf("Incoming bytes to compress: %08b", bytes)
	totalBytes := ((pLength + sLength) / 2) + 1
	newBytes := make([]byte, totalBytes)

	targetIndex := 0
	targetLeftHalf := true
	var newByte byte = 0
	for targetIndex < totalBytes {
		for pIndex := range pLength {
			log.Printf("P %d: %08b", pIndex, bytes[pIndex])
			if targetLeftHalf {
				newByte = bytes[pIndex] << 4
			} else {
				newByte = newByte | bytes[pIndex]
				newBytes[targetIndex] = newByte
				log.Printf("Setting %d, New Bytes: %08b", targetIndex, newBytes)
				targetIndex++
			}
			targetLeftHalf = !targetLeftHalf
		}
		for sIndex := range sLength {
			log.Printf("S %d: %08b", sIndex, bytes[pLength+sIndex])
			if targetLeftHalf {
				newByte = bytes[pLength+sIndex] << 4
			} else {
				newByte = newByte | bytes[pLength+sIndex]
				newBytes[targetIndex] = newByte
				log.Printf("Setting %d, New Bytes: %08b", targetIndex, newBytes)
				targetIndex++
			}
			targetLeftHalf = !targetLeftHalf
		}
		// Add sign
		var sign byte
		if isNegative {
			sign = 13
		} else {
			sign = 12
		}
		if targetLeftHalf {
			newBytes[targetIndex] = sign
		} else {
			newBytes[targetIndex] = newByte | sign
		}
		log.Printf("Setting %d, New Bytes: %08b", targetIndex, newBytes)
		targetIndex++

	}
	log.Printf("Compressed to %08b", newBytes)
	return newBytes
}

func parseFieldData(field *Field, data []byte, fieldStartPos int32) int32 {
	m := map[uint8]string{
		0: "", 1: "", 2: "", 3: "", 4: "",
		5: "", 6: "", 7: "", 8: "", 9: "",
		10: "", 11: "", 12: "", 13: "", 14: "",
		15: "", 16: "", 17: "", 18: "", 19: "",
		20: "", 21: "", 22: "", 23: "", 24: "",
		25: "", 26: "", 27: "", 28: "", 29: "",
		30: "", 31: "", 32: "", 33: "", 34: "",
		35: "", 36: "", 37: "", 38: "", 39: "",
		40: "", 41: "", 42: "", 43: "", 44: "",
		45: "", 46: "", 47: "", 48: "", 49: "",
		50: "", 51: "", 52: "", 53: "", 54: "",
		55: "", 56: "", 57: "", 58: "", 59: "",
		60: "", 61: "", 62: "", 63: "", 64: "",
		65: "", 66: "â", 67: "ä", 68: "à", 69: "á",
		70: "ã", 71: "å", 72: "ç", 73: "ñ", 74: "¢",
		75: ".", 76: "<", 77: "(", 78: "+", 79: "|",
		80: "&", 81: "é", 82: "ê", 83: "ë", 84: "è",
		85: "í", 86: "î", 87: "ï", 88: "ì", 89: "ß",
		90: "!", 91: "$", 92: "*", 93: ")", 94: ";",
		95: "¬", 96: "-", 97: "/", 98: "Â", 99: "Ä",
		100: "À", 101: "Á", 102: "Ã", 103: "Å", 104: "Ç",
		105: "Ñ", 106: "¦", 107: ",", 108: "%", 109: "_",
		110: ">", 111: "?", 112: "ø", 113: "É", 114: "Ê",
		115: "Ë", 116: "È", 117: "Í", 118: "Î", 119: "Ï",
		120: "Ì", 121: "`", 122: ":", 123: "#", 124: "@",
		125: "'", 126: "=", 127: "\"", 128: "Ø", 129: "a",
		130: "b", 131: "c", 132: "d", 133: "e", 134: "f",
		135: "g", 136: "h", 137: "i", 138: "«", 139: "»",
		140: "ð", 141: "ý", 142: "þ", 143: "±", 144: "°",
		145: "j", 146: "k", 147: "l", 148: "m", 149: "n",
		150: "o", 151: "p", 152: "q", 153: "r", 154: "ª",
		155: "º", 156: "æ", 157: "¸", 158: "Æ", 159: "¤",
		160: "µ", 161: "~", 162: "s", 163: "t", 164: "u",
		165: "v", 166: "w", 167: "x", 168: "y", 169: "z",
		170: "¡", 171: "¿", 172: "Ð", 173: "Ý", 174: "Þ",
		175: "®", 176: "^", 177: "£", 178: "¥", 179: "·",
		180: "©", 181: "§", 182: "¶", 183: "¼", 184: "½",
		185: "¾", 186: "[", 187: "]", 188: "¯", 189: "¨",
		190: "´", 191: "×", 192: "{", 193: "A", 194: "B",
		195: "C", 196: "D", 197: "E", 198: "F", 199: "G",
		200: "H", 201: "I", 202: "­", 203: "ô", 204: "ö",
		205: "ò", 206: "ó", 207: "õ", 208: "}", 209: "J",
		210: "K", 211: "L", 212: "M", 213: "N", 214: "O",
		215: "P", 216: "Q", 217: "R", 218: "¹", 219: "û",
		220: "ü", 221: "ù", 222: "ú", 223: "ÿ", 224: "\"",
		225: "÷", 226: "S", 227: "T", 228: "U", 229: "V",
		230: "W", 231: "X", 232: "Y", 233: "Z", 234: "²",
		235: "Ô", 236: "Ö", 237: "Ò", 238: "Ó", 239: "Õ",
		240: "0", 241: "1", 242: "2", 243: "3", 244: "4",
		245: "5", 246: "6", 247: "7", 248: "8", 249: "9",
		250: "³", 251: "Û", 252: "Ü", 253: "Ù", 254: "Ú",
		255: "",
	}

	if field.fieldType == ANY_CHAR {
		field.startPos = fieldStartPos
		field.rawData = data[field.startPos : field.startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range field.rawData {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + m[datumByte]
		}
		log.Printf("%d -> %d -> %s\n", field.rawData, byteSlice, stringBuffer)
		fieldStartPos += field.length
		field.Data = stringBuffer
	} else if field.fieldType == DECIMAL {
		var byteSlice []uint8
		var stringBuffer string = ""
		// parse the left side of the decimal
		// if the pLength is even, we have 2 4 bits per byte, we need half the bytes
		// if the pLength is odd, we round down, and set the extra 4 bit flag
		var endPos int32 = 0
		extra4BitFlag := false
		if field.pLength%2 == 0 {
			endPos = field.pLength / 2
		} else {
			endPos = (field.pLength - 1) / 2
			extra4BitFlag = true
		}
		field.startPos = fieldStartPos
		field.rawData = data[field.startPos : field.startPos+(((field.sLength+field.pLength)/2)+1)]
		for j, datumByte := range data[fieldStartPos : fieldStartPos+endPos] {
			byteSlice = append(byteSlice, datumByte)
			high4Bits := datumByte >> 4
			stringBuffer = stringBuffer + strconv.Itoa(int(high4Bits))
			low4Bits := datumByte & (8 + 4 + 2 + 1)
			stringBuffer = stringBuffer + strconv.Itoa(int(low4Bits))
			log.Printf("High: %08b, Low: %08b", high4Bits, low4Bits)
			log.Printf("%d: %d -> %08b -> %s\n", j, datumByte, byteSlice, stringBuffer)
		}
		fieldStartPos = fieldStartPos + endPos
		if extra4BitFlag {
			log.Printf("We need the first 4 bits only of: %08b", data[fieldStartPos])
			byteSlice = append(byteSlice, data[fieldStartPos])
			high4Bits := data[fieldStartPos] >> 4
			stringBuffer = stringBuffer + strconv.Itoa(int(high4Bits))
			log.Printf("?: %d -> %08b -> %s\n", data[fieldStartPos], byteSlice, stringBuffer)
		}

		stringBuffer = stringBuffer + "."
		sIntCount := int32(0)
		var stringAsDecimal decimal.Decimal
		// parse the right side of the decimal
		for j, datum := range data[fieldStartPos : fieldStartPos+field.sLength] {
			sIntCount++
			datumConverted := datum
			byteSlice = append(byteSlice, datumConverted)

			// we need to get the bottom 4 bits from this first number only
			if extra4BitFlag {
				low4Bits := datum & (8 + 4 + 2 + 1)
				stringBuffer = stringBuffer + strconv.Itoa(int(low4Bits))
				// reset flag so it doesn't catch next iteration
				extra4BitFlag = false
			} else if sIntCount != field.sLength {
				// if not the last element, just append
				byteSlice = append(byteSlice, datum)
				high4Bits := datum >> 4
				stringBuffer = stringBuffer + strconv.Itoa(int(high4Bits))

				low4Bits := datum & (8 + 4 + 2 + 1)
				stringBuffer = stringBuffer + strconv.Itoa(int(low4Bits))
				log.Printf("High: %08b, Low: %08b", high4Bits, low4Bits)
				log.Printf("%d: %d -> %08b -> %s\n", j, datum, byteSlice, stringBuffer)
			} else {
				log.Printf("Last 8 bytes: %08b", datum)
				// this is the last byte, so we need to parse it special like
				// the top 4 bits we want
				high4Bits := datum >> 4
				stringBuffer = stringBuffer + strconv.Itoa(int(high4Bits))

				low4Bits := datum & (8 + 4 + 2 + 1)
				log.Printf("Sign: %08b", low4Bits)
				//if the lowbits  == 13 its a negative sign
				if low4Bits == 13 {
					stringBuffer = "-" + stringBuffer
				}
			}
			var err error
			stringAsDecimal, err = decimal.NewFromString(stringBuffer)
			if err != nil {
				panic(err)
			}
			log.Printf("%d: %d -> %08b-> %s -> %v\n", j, datum, byteSlice, stringBuffer, stringAsDecimal)
		}
		log.Printf("%08b -> %s -> %v\n", byteSlice, stringBuffer, stringAsDecimal)
		field.Data = stringAsDecimal
	} else if field.fieldType == SIGNED_BINARY {
		field.startPos = fieldStartPos
		datum := data[field.startPos : field.startPos+field.length]

		//isLessThan4Bytes := false
		signBit := datum[0] >> 7
		log.Printf("Sign Bit: %b\n", signBit)

		var num = make([]int8, len(datum))
		for i := range datum {
			num[i] = int8(datum[i])
		}

		log.Printf("bytes: %08b -> datum: %d -> %d\n", datum, datum, num)
		field.Data = num
		fieldStartPos += field.length
		field.rawData = datum
	} else if field.fieldType == UNSIGNED_BINARY {
		field.startPos = fieldStartPos
		datum := data[field.startPos : field.startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range datum {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + strconv.Itoa(int(datumByte))
		}

		log.Printf("%s: %08b -> %08b -> %08b -> %s\n", field.label, datum, byteSlice, byteSlice, stringBuffer)
		field.Data = byteSlice
		fieldStartPos += field.length
		field.rawData = byteSlice
	} else if field.fieldType == FLOAT4 {
		field.startPos = fieldStartPos
		exponent := data[field.startPos : field.startPos+3]
		field.startPos += 3
		mantissa := data[field.startPos : field.startPos+1]
		field.startPos += 1

		var exponentByte []byte
		for _, val := range exponent {
			exponentByte = append(exponentByte, byte(val))
		}

		var mantissaByte []byte
		for _, val := range mantissa {
			mantissaByte = append(mantissaByte, byte(val))
		}

		log.Printf("Exponent: %d -> %08b, %d -> %08b", exponent, exponent, exponentByte, exponentByte)
		log.Printf("Mantissa: %d -> %08b, %d -> %08d", mantissa, mantissa, mantissaByte, mantissaByte)

		var dataByte []byte
		dataByte = append(dataByte, exponentByte...)
		dataByte = append(dataByte, mantissaByte...)

		bits := binary.LittleEndian.Uint32(dataByte)
		floatVal := math.Float32frombits(bits)
		log.Printf("%v -> %d -> %08b\n", floatVal, dataByte, bits)
		field.Data = floatVal
		fieldStartPos = field.startPos
		field.rawData = dataByte
	} else if field.fieldType == ALPHA_CHAR {
		field.startPos = fieldStartPos
		field.rawData = data[field.startPos : field.startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range field.rawData {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + m[datumByte]
		}
		log.Printf("%d -> %d -> %s\n", field.rawData, byteSlice, stringBuffer)
		fieldStartPos += field.length
		field.Data = stringBuffer
	} else if field.fieldType == NUM_CHAR {
		field.startPos = fieldStartPos
		datum := data[fieldStartPos : fieldStartPos+field.length]
		field.rawData = data[field.startPos : field.startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range datum {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + m[datumByte]
		}
		log.Printf("%d -> %d -> %s\n", datum, datum, stringBuffer)
		fieldStartPos += field.length
		field.Data = stringBuffer
	}
	return fieldStartPos
	//return startPos, field.Data

}

func ParseBinaryData(fileStruct *File, data []byte) {
	recordStartPos := fileStruct.StartPos
	fieldStartPos := recordStartPos
	for i := range fileStruct.Fields {
		fieldStartPos = parseFieldData(&fileStruct.Fields[i], data, fieldStartPos)
		//fileStruct.Fields[i].startPos = startPos
		//fileStruct.Fields[i].Data = dataResult
		log.Printf("i=%d, label=%s, length=%d, startPos=%d, rawData=%08b\n", i, fileStruct.Fields[i].label, fileStruct.Fields[i].length, fileStruct.Fields[i].startPos, fileStruct.Fields[i].GetRawData())
	}
	fileStruct.StartPos = fileStruct.StartPos + fileStruct.RecordLength
}

func GetBinaryData(fileStruct *File) []byte {
	var data []byte
	for i, v := range fileStruct.Fields {
		log.Printf("i=%d, label=%s, length=%d, startPos=%d, rawData=%08b\n", i, v.label, v.length, v.startPos, v.GetRawData())
		data = append(data, v.rawData...)
	}
	fileStruct.StartPos = fileStruct.StartPos + fileStruct.RecordLength
	return data
}
