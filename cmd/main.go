package main

import (
	"fmt"
	"log"
	"os"

	"github.com/retinaburn/cobolparser/parser"
	"github.com/shopspring/decimal"
)

func getLexer(fileName string) *parser.Lexer {
	file, err := os.Open(fileName)
	if err != nil {
		panic(err)
	}

	lexer := parser.NewLexer(file)
	return lexer
}

func getBytes(fileName string) ([]byte, int) {
	f, err := os.Open(fileName)
	if err != nil {
		panic(err)
	}
	var readBytes = make([]byte, 256)

	len, err := f.Read(readBytes)
	if err != nil {
		panic(err)
	}
	return readBytes, len
}

func printField(fileStruct *parser.File, fieldLabel string) {
	field, err := fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	log.Printf("Type: %T", field.Data)
	switch field.Data.(type) {
	case int32:
		fmt.Printf("Data by field %s pull: %d\n", fieldLabel, field.Data)
	case int:
		fmt.Printf("Data by field %s pull: %d\n", fieldLabel, field.Data)
	case string:
		fmt.Printf("Data by field %s pull: %s\n", fieldLabel, field.Data)
	case float32:
		fmt.Printf("Data by field %s pull: %v\n", fieldLabel, field.Data)
	case decimal.Decimal:
		fmt.Printf("Data by field %s pull: %v\n", fieldLabel, field.Data)
		fmt.Printf("Raw Data: %08b\n", field.GetRawData())
	case []uint8:
		fmt.Printf("Data by field %s pull: %v\n", fieldLabel, field.Data)

	default:
		fmt.Printf("Unsupported type in printField: %T, data: %08b", field.Data, field.GetRawData())
	}

}

func number() {

	lexer := getLexer("resources/number.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/number.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID"
	printField(&fileStruct, fieldLabel)

}
func string2() {

	lexer := getLexer("resources/string.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/string.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID"
	printField(&fileStruct, fieldLabel)

}
func largedecimal() {

	lexer := getLexer("resources/largedecimal.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/largedecimal-pos.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "AVAIL-BAL"
	printField(&fileStruct, fieldLabel)

	lexer = getLexer("resources/largedecimal.copybook")
	fileStruct = parser.ParseLexData(lexer)
	readBytes, len = getBytes("resources/largedecimal-neg.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	printField(&fileStruct, fieldLabel)
}
func unsignedComposite() {

	lexer := getLexer("resources/unsigned-composite.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/unsigned-composite.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID-1"
	printField(&fileStruct, fieldLabel)
}

func float() {

	lexer := getLexer("resources/float.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/float.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID-1"
	printField(&fileStruct, fieldLabel)
}

func alpha() {

	lexer := getLexer("resources/alpha.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/alpha.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID-1"
	printField(&fileStruct, fieldLabel)
}

func signedcomposite() {

	lexer := getLexer("resources/signed-composite.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/signed-composite.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID-1"
	printField(&fileStruct, fieldLabel)
}

func setalpha() {

	lexer := getLexer("resources/alpha.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/alpha.ebcdic")

	log.Printf("Read %d bytes", len)
	originalRawData := make([]byte, len)
	copy(originalRawData, readBytes[0:len])
	log.Printf("Raw Data: %08b", originalRawData)

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID-1"
	printField(&fileStruct, fieldLabel)

	field, err := fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	err = field.SetData(1234)
	if err != nil {
		log.Println(err)
	}
	err = field.SetData("B")
	if err != nil {
		log.Println(err)
	}
	log.Printf("Original Data: %08b", originalRawData)
	log.Printf("Modified Data: %08b", parser.GetBinaryData(&fileStruct))

	fieldLabel = "TRANS-ID-2"
	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	err = field.SetData("CD")
	if err != nil {
		log.Println(err)
	}
	log.Printf("Original Data: %08b", originalRawData)
	log.Printf("Modified Data: %08b", parser.GetBinaryData(&fileStruct))

	printField(&fileStruct, fieldLabel)

	//field.SetData("CDE") //logs error and exits
}

func setany() {

	lexer := getLexer("resources/string.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/string.ebcdic")

	log.Printf("Read %d bytes", len)
	originalRawData := make([]byte, len)
	copy(originalRawData, readBytes[0:len])
	log.Printf("Raw Data: %08b", originalRawData)

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID"
	printField(&fileStruct, fieldLabel)

	field, err := fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	err = field.SetData("AB")
	if err != nil {
		panic(err)
	}
	err = field.SetData("YZ")
	if err != nil {
		panic(err)
	}
	log.Printf("Original Data: %08b", originalRawData)
	log.Printf("Modified Data: %08b", parser.GetBinaryData(&fileStruct))

	printField(&fileStruct, fieldLabel)

	//field.SetData("CDE") //logs error and exits
}

func setnumber() {

	lexer := getLexer("resources/number.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/number.ebcdic")

	log.Printf("Read %d bytes", len)
	originalRawData := make([]byte, len)
	copy(originalRawData, readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID"
	printField(&fileStruct, fieldLabel)

	field, err := fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	err = field.SetData("1")
	if err != nil {
		panic(err)
	}
	err = field.SetData("2")
	if err != nil {
		panic(err)
	}
	log.Printf("Original Data: %08b", originalRawData)
	log.Printf("Modified Data: %08b", parser.GetBinaryData(&fileStruct))

	printField(&fileStruct, fieldLabel)
}

func setlargedecimal() {

	lexer := getLexer("resources/largedecimal.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/largedecimal-pos.ebcdic")

	log.Printf("Read %d bytes", len)
	originalRawData := make([]byte, len)
	copy(originalRawData, readBytes[0:len])
	fmt.Printf("Original Data: %08b\n", originalRawData[0:len])
	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "AVAIL-BAL"
	printField(&fileStruct, fieldLabel)

	field, err := fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}

	newVal := "1.01"
	fmt.Printf("Setting to %s\n", newVal)
	stringAsDecimal, err := decimal.NewFromString(newVal)
	if err != nil {
		log.Fatal("failed to cast string as decimal")
	}
	err = field.SetData(stringAsDecimal)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Reparse\n")
	updatedBytes := parser.GetBinaryData(&fileStruct)
	fmt.Printf("Modified Data: %08b", updatedBytes)

	lexer = getLexer("resources/largedecimal.copybook")
	fileStruct = parser.ParseLexData(lexer)
	parser.ParseBinaryData(&fileStruct, updatedBytes[0:len])
	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	log.Printf("%08b", field.GetRawData())
	printField(&fileStruct, fieldLabel)

	newVal = "-2.02"
	fmt.Printf("Setting to %s\n", newVal)
	stringAsDecimal, err = decimal.NewFromString(newVal)
	if err != nil {
		log.Fatal("failed to cast string as decimal")
	}
	err = field.SetData(stringAsDecimal)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Reparse\n")
	updatedBytes = parser.GetBinaryData(&fileStruct)
	fmt.Printf("Modified Data: %08b", updatedBytes)

	lexer = getLexer("resources/largedecimal.copybook")
	fileStruct = parser.ParseLexData(lexer)
	parser.ParseBinaryData(&fileStruct, updatedBytes[0:len])
	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	log.Printf("%08b", field.GetRawData())
	printField(&fileStruct, fieldLabel)
}

func setunsignedComposite() {

	lexer := getLexer("resources/unsigned-composite.copybook")
	fileStruct := parser.ParseLexData(lexer)

	readBytes, len := getBytes("resources/unsigned-composite.ebcdic")

	log.Printf("Read %d bytes", len)
	log.Printf("%08b", readBytes[0:len])

	parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	var fieldLabel = "TRANS-ID-1"
	printField(&fileStruct, fieldLabel)

	field, err := fileStruct.Field(fieldLabel)

	if err != nil {
		panic(err)
	}
	err = field.SetData([]uint8{2, 0})
	if err != nil {
		panic(err)
	}

	fmt.Printf("Reparse\n")
	updatedBytes := parser.GetBinaryData(&fileStruct)
	fmt.Printf("Modified Data: %08b", updatedBytes)

	lexer = getLexer("resources/unsigned-composite.copybook")
	fileStruct = parser.ParseLexData(lexer)
	parser.ParseBinaryData(&fileStruct, updatedBytes[0:len])
	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	log.Printf("%08b", field.GetRawData())
	printField(&fileStruct, fieldLabel)

	err = field.SetData([]uint8{0, 255})
	if err != nil {
		panic(err)
	}
	fmt.Printf("Reparse\n")
	updatedBytes = parser.GetBinaryData(&fileStruct)
	fmt.Printf("Modified Data: %08b", updatedBytes)

	lexer = getLexer("resources/unsigned-composite.copybook")
	fileStruct = parser.ParseLexData(lexer)
	parser.ParseBinaryData(&fileStruct, updatedBytes[0:len])
	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	log.Printf("%08b", field.GetRawData())
	printField(&fileStruct, fieldLabel)

	fieldLabel = "TRANS-ID-20"
	printField(&fileStruct, fieldLabel)

	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	err = field.SetData([]uint8{255, 255, 255, 255, 255, 255, 255, 255})
	if err != nil {
		panic(err)
	}

	fmt.Printf("Reparse\n")
	updatedBytes = parser.GetBinaryData(&fileStruct)
	fmt.Printf("Modified Data: %08b", updatedBytes)

	lexer = getLexer("resources/unsigned-composite.copybook")
	fileStruct = parser.ParseLexData(lexer)
	parser.ParseBinaryData(&fileStruct, updatedBytes[0:len])
	field, err = fileStruct.Field(fieldLabel)
	if err != nil {
		panic(err)
	}
	log.Printf("%08b", field.GetRawData())
	printField(&fileStruct, fieldLabel)
}

func main() {

	// number()
	// string2()
	//largedecimal()
	//unsignedComposite()
	//float()
	//alpha()
	//signedcomposite()

	//setalpha()
	//setany()
	//setnumber()
	//setlargedecimal()
	setunsignedComposite()

}
