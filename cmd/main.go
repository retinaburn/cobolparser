package main

import (
	"fmt"
	"log"
	"os"

	"github.com/retinaburn/cobolparser/parser"
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
		fmt.Printf("Data by field %s index: %d\n", fieldLabel, (fileStruct.Fields[0]).Data)
	case int:
		fmt.Printf("Data by field %s pull: %d\n", fieldLabel, field.Data)
		fmt.Printf("Data by field %s index: %d\n", fieldLabel, (fileStruct.Fields[0]).Data)
	case string:
		fmt.Printf("Data by field %s pull: %s\n", fieldLabel, field.Data)
		fmt.Printf("Data by field %s index: %s\n", fieldLabel, (fileStruct.Fields[0]).Data)
	case float32:
		fmt.Printf("Data by field %s pull: %v\n", fieldLabel, field.Data)
		fmt.Printf("Data by field %s index: %v\n", fieldLabel, (fileStruct.Fields[0]).Data)
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

func main() {

	// number()
	// string2()
	//largedecimal()
	//unsignedComposite()
	//float()
	//alpha()
	signedcomposite()

}
