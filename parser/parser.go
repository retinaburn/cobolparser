package parser

import (
	"fmt"
	"log"
	"regexp"
	"strconv"

	"github.com/shopspring/decimal"
)

type PicType int

const (
	SIGNED_BINARY PicType = iota
	UNSIGNED_BINARY
	FLOAT
	ALPHA_CHAR
	ANY_CHAR
	NUM_CHAR //PIC X(n)
	DECIMAL  //PIC S9(p)V9(s) COMP-3
	DISPLAY_NUMERIC
)

type Field struct {
	label     string
	length    int32
	sLength   int32
	pLength   int32
	fieldType PicType
}

func newFieldForString(label string, length int32, fieldType PicType) Field {
	f := Field{}
	f.label = label
	f.length = length
	f.fieldType = fieldType
	return f
}

func newFieldForDecimal(label string, s int32, p int32, fieldType PicType) Field {
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
	if length <= 3 {
		f.length = 1
	} else if length <= 5 {
		f.length = 2
	} else if length <= 10 {
		f.length = 5
	} else if length <= 19 {
		f.length = 10
	}
	f.fieldType = fieldType
	return f
}

func ParseLexData(lexer *Lexer) []Field {
	var fields []Field

	stringREString := `^PIC X\((\d+)\)$`
	stringRE := regexp.MustCompile(stringREString)
	decimalREString := `^PIC S9\((\d+)\)V9\((\d+)\) COMP-3$`
	decimalRE := regexp.MustCompile(decimalREString)
	numberREString := `^PIC S9\((\d+)\) COMP$|^PIC S9 COMP$`
	numberRE := regexp.MustCompile(numberREString)

	var lastIdent string
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
				foundLength, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				fields = append(fields, newFieldForString(lastIdent, int32(foundLength), ANY_CHAR))
			}

			// Capture Decimal Type "PIC S9(p)V9(s) COMP-3"
			capGroups = decimalRE.FindStringSubmatch(lit)
			if len(capGroups) > 0 {
				foundPLength, err := strconv.Atoi(capGroups[2])
				if err != nil {
					panic(err)
				}
				foundSLength, err := strconv.Atoi(capGroups[1])
				if err != nil {
					panic(err)
				}
				fields = append(fields, newFieldForDecimal(lastIdent, int32(foundPLength), int32(foundSLength), DECIMAL))
			}

			// Capture Numeric Type "PIC S9(p) COMP" or "PIC S9 COMP"
			capGroups = numberRE.FindStringSubmatch(lit)
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
				fields = append(fields, newFieldForSignedBinary(lastIdent, int32(length), SIGNED_BINARY))
			}
		}

		fmt.Printf("%d:%d\t|%s|\t|%s|\n", pos.line, pos.column, tok, lit)
	}
	return fields
}

func ParseData(fields []Field, data []int) []Field {
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

	startPos := int32(0)
	for i, v := range fields {
		log.Printf("i=%d, label=%s, length=%d:\n", i, v.label, v.length)
		if v.fieldType == ANY_CHAR {
			datum := data[startPos : startPos+v.length]
			var byteSlice []uint8
			var stringBuffer string = ""

			for _, datumInt := range datum {
				byteSlice = append(byteSlice, (uint8)(datumInt+256))
				stringBuffer = stringBuffer + m[(uint8)(datumInt+256)]
			}
			fmt.Printf("%d -> %d -> %s\n", datum, byteSlice, stringBuffer)
			startPos += v.length
		} else if v.fieldType == DECIMAL {
			var byteSlice []uint8
			var stringBuffer string = ""
			// parse the left side of the decimal
			for j, datum := range data[startPos : startPos+v.pLength] {
				byteSlice = append(byteSlice, (uint8)(datum+256))
				stringBuffer = stringBuffer + m[(uint8)(datum+256)]
				log.Printf("%d: %d -> %d -> %s\n", j, datum, byteSlice, stringBuffer)
			}
			startPos += v.pLength
			stringBuffer = stringBuffer + "."
			sIntCount := int32(0)
			var stringAsDecimal decimal.Decimal
			// parse the right side of the decimal
			for j, datum := range data[startPos : startPos+v.sLength] {
				sIntCount++
				datumConverted := (uint8)(datum + 256)
				byteSlice = append(byteSlice, datumConverted)
				log.Printf("%08b\n", datumConverted)
				// if not the last element, just append
				if sIntCount != v.sLength {
					stringBuffer = stringBuffer + m[datumConverted]
				} else {
					// this is the last byte, so we need to parse it special like
					// the bottom 4 bits are the number we want
					// so bitwise & to get just the low bits
					lowBits := datumConverted & (8 + 4 + 2 + 1)
					// make the high bits 240, to shift it up the code sheet
					lowBits = lowBits | (128 + 64 + 32 + 16)
					log.Printf("Low: %08b\n", lowBits)
					//append the new low byte to the string
					stringBuffer = stringBuffer + m[lowBits]

					// to get the sign (there is definitely and easier way with java bytes...)
					// shift 4 bits to the right, to get rid of the low bits
					highBits := datumConverted >> 4
					log.Printf("High: %08b : %d\n", highBits, highBits)
					signInt := int8(highBits)
					log.Printf("Sign: %d\n", signInt)
					//if the highBits (shifted) == 13 its a negative sign
					if signInt == 13 {
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
		} else if v.fieldType == SIGNED_BINARY {
			datum := data[startPos : startPos+v.length]
			var byteSlice []int8
			var stringBuffer string = ""

			for _, datumInt := range datum {
				byteSlice = append(byteSlice, (int8)(datumInt+256))
				stringBuffer = stringBuffer + m[(uint8)(datumInt+256)]
			}
			fmt.Printf("%d -> %d -> %08b\n", datum, byteSlice, byteSlice)
			startPos += v.length
		}
	}

	return fields
}
