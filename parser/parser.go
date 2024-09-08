package parser

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"log"
	"math"
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

type Field struct {
	label     string
	length    int32
	sLength   int32
	pLength   int32
	fieldType PicType
	startPos  int32
	Data      any
}

type File struct {
	RecordLength int32
	Fields       []Field
	StartPos     int32
}

func (f *File) Field(fieldName string) (*Field, error) {
	for i, field := range f.Fields {
		if field.label == fieldName {
			log.Printf("Found field %s: %d, %s, %d, %s", fieldName, i, field.label, field.startPos, field.Data)
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

		fmt.Printf("%d:%d\t|%s|\t|%s|\n", pos.line, pos.column, tok, lit)
	}
	return file
}

func parseFieldData(field *Field, data []byte, startPos int32) (int32, any) {
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
		datum := data[startPos : startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range datum {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + m[datumByte]
		}
		fmt.Printf("%d -> %d -> %s\n", datum, byteSlice, stringBuffer)
		startPos += field.length
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
		for j, datumByte := range data[startPos : startPos+endPos] {
			byteSlice = append(byteSlice, datumByte)
			high4Bits := datumByte >> 4
			stringBuffer = stringBuffer + strconv.Itoa(int(high4Bits))
			low4Bits := datumByte & (8 + 4 + 2 + 1)
			stringBuffer = stringBuffer + strconv.Itoa(int(low4Bits))
			log.Printf("High: %08b, Low: %08b", high4Bits, low4Bits)
			log.Printf("%d: %d -> %d -> %s\n", j, datumByte, byteSlice, stringBuffer)
		}
		startPos = startPos + endPos
		if extra4BitFlag {
			log.Printf("We need the first 4 bits only of: %08b", data[startPos])
			byteSlice = append(byteSlice, data[startPos])
			high4Bits := data[startPos] >> 4
			stringBuffer = stringBuffer + strconv.Itoa(int(high4Bits))
			log.Printf("?: %d -> %d -> %s\n", data[startPos], byteSlice, stringBuffer)
		}

		stringBuffer = stringBuffer + "."
		sIntCount := int32(0)
		var stringAsDecimal decimal.Decimal
		// parse the right side of the decimal
		for j, datum := range data[startPos : startPos+field.sLength] {
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
				log.Printf("%d: %d -> %d -> %s\n", j, datum, byteSlice, stringBuffer)
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
			log.Printf("%d: %d -> %d -> %s -> %v\n", j, datum, byteSlice, stringBuffer, stringAsDecimal)
		}
		fmt.Printf("%d -> %s -> %v\n", byteSlice, stringBuffer, stringAsDecimal)
		field.Data = stringAsDecimal
	} else if field.fieldType == SIGNED_BINARY {
		datum := data[startPos : startPos+field.length]

		isLessThan4Bytes := false
		signBit := datum[0] >> 7
		log.Printf("Sign Bit: %b\n", signBit)

		// We have to handle data lengths of 2 bytes specially
		// because the binary.Read can't operate on them natively
		if len(datum) < 4 {
			isLessThan4Bytes = true
			datum4Byte := make([]byte, 4)
			datum4Byte[2] = datum[0]
			datum4Byte[3] = datum[1]
			datum = datum4Byte
		}

		var num int32
		err := binary.Read(bytes.NewReader(datum), binary.BigEndian, &num)
		if err != nil {
			fmt.Println(err)
		}
		if isLessThan4Bytes && signBit == 1 {
			newNum := (65536 - num) * -1
			log.Printf("Num: %d -> New Num: %d", num, newNum)
			num = newNum
		}

		log.Printf("bytes: %08b -> int: %d %d -> %d\n", datum, int(datum[0]), int(datum[1]), num)
		field.Data = num
		startPos += field.length
	} else if field.fieldType == UNSIGNED_BINARY {
		datum := data[startPos : startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range datum {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + strconv.Itoa(int(datumByte))
		}
		result, err := strconv.Atoi(stringBuffer)
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s: %d -> %d -> %08b -> %s -> %d\n", field.label, datum, byteSlice, byteSlice, stringBuffer, result)
		field.Data = result
		startPos += field.length
		return startPos, field.Data
	} else if field.fieldType == FLOAT4 {
		exponent := data[startPos : startPos+1]
		startPos += 1
		mantissa := data[startPos : startPos+3]
		startPos += 3

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
		dataByte = append(dataByte, exponentByte[0])
		dataByte = append(dataByte, mantissaByte...)

		bits := binary.LittleEndian.Uint32(dataByte)
		floatVal := math.Float32frombits(bits)
		fmt.Printf("%v -> %d -> %08b\n", floatVal, dataByte, bits)
		field.Data = floatVal
	} else if field.fieldType == ALPHA_CHAR {
		datum := data[startPos : startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range datum {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + m[datumByte]
		}
		fmt.Printf("%d -> %d -> %s\n", datum, byteSlice, stringBuffer)
		startPos += field.length
		field.Data = stringBuffer
	} else if field.fieldType == NUM_CHAR {
		datum := data[startPos : startPos+field.length]
		var byteSlice []uint8
		var stringBuffer string = ""

		for _, datumByte := range datum {
			byteSlice = append(byteSlice, datumByte)
			stringBuffer = stringBuffer + m[datumByte]
		}
		fmt.Printf("%d -> %d -> %s\n", datum, datum, stringBuffer)
		startPos += field.length
		field.Data = stringBuffer
	}
	return startPos, field.Data

}

func increment(datum []byte) []byte {
	datumIndex := 0
	//datum = ^ datum

	//datum[0] = datum[0] + 1
	bit0 := datum[datumIndex] & 128 >> 7
	bit1 := datum[datumIndex] & 64 >> 6
	bit2 := datum[datumIndex] & 32 >> 5
	bit3 := datum[datumIndex] & 16 >> 4
	bit4 := datum[datumIndex] & 8 >> 3
	bit5 := datum[datumIndex] & 4 >> 2
	bit6 := datum[datumIndex] & 2 >> 1
	bit7 := datum[datumIndex] & 1
	log.Printf("byte-0: 0:%b,1:%b,2:%b,3:%b,4:%b,5:%b,6:%b,7:%b",
		bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)
	return datum
}

func ParseBinaryData(fileStruct *File, data []byte) {

	startPos := fileStruct.StartPos
	var dataResult any
	for i, v := range fileStruct.Fields {
		startPos, dataResult = parseFieldData(&v, data, startPos)
		fileStruct.Fields[i].startPos = startPos
		fileStruct.Fields[i].Data = dataResult
		log.Printf("i=%d, label=%s, length=%d, startPos=%d\n", i, v.label, v.length, v.startPos)
	}
	fileStruct.StartPos = fileStruct.StartPos + fileStruct.RecordLength
}
