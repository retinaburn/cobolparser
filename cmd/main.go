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
	fmt.Printf("Data by field %s pull: %s\n", fieldLabel, field.Data)
	fmt.Printf("Data by field %s index: %s\n", fieldLabel, (fileStruct.Fields[0]).Data)

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
func main() {

	// number()
	// string2()
	largedecimal()
	//file, err := os.Open("resources/unsigned-binary.copybook")
	//file, err := os.Open("resources/unsigned-binary.copybook")
	//file, err := os.Open("resources/float.copybook")
	//file, err := os.Open("resources/alpha.copybook")
	// file, err := os.Open("resources/number.copybook")
	// if err != nil {
	// 	panic(err)
	// }

	// lexer := parser.NewLexer(file)
	// fileStruct := parser.ParseLexData(lexer)

	// Data for string2
	//javaData := []int{-63, -62, -128, -127, -126, -125, -124, -123, -122, -121, -120, -119}

	// Data for largedecimal
	//100.99
	//javaData := []int{-16, -16, -16, -16, -16, -16, -16, -16, -15, -16, -16, -7, -55}
	//-100.99
	//javaData := []int{-16, -16, -16, -16, -16, -16, -16, -16, -15, -16, -16, -7, -39}

	// Data for signed binary
	//javaData := []int{1, 1, -64, 64, 10, -1, 127}

	// Data for unsigned binary
	// javaData := []int{1, 1, -64, 64, 10, -1, 127, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}

	// Data for float
	//javaData := []int{111, -51, -113, 63}
	//javaData := []int{-6, -40, 27, 95}

	// Data for Alpha
	// fmt.Printf("Record length: %d\n", fileStruct.RecordLength)
	// javaData := []int{-15, -14, -13, -12, -11, -10, -9, -8, -7, -15, -14, -13}

	// // Parse first record
	// parser.ParseData(&fileStruct, javaData)
	// fmt.Printf("Current data start position: %d\n", fileStruct.StartPos)
	// fmt.Printf("Fields: %s\n", fileStruct.FieldNames())
	// field, err := fileStruct.Field("TRANS-ID")
	// if err != nil {
	// 	panic(err)
	// }
	// fmt.Printf("Data by field pull: %s\n", field.Data)
	// fmt.Printf("Data by field index: %s\n", (fileStruct.Fields[0]).Data)

	// // Parse next record
	// parser.ParseData(&fileStruct, javaData)
	// fmt.Printf("Current data start position: %d\n", fileStruct.StartPos)

	// f, err := os.Open("resources/number.ebcdic")
	// if err != nil {
	// 	panic(err)
	// }
	// var readBytes = make([]byte, 256)

	// len, err := f.Read(readBytes)
	// if err != nil {
	// 	panic(err)
	// }
	// log.Printf("Read %d bytes", len)
	// log.Printf("%08b", readBytes[0:len])

	// parser.ParseBinaryData(&fileStruct, readBytes[0:len])

	// field, err := fileStruct.Field("TRANS-ID")
	// if err != nil {
	// 	panic(err)
	// }
	// fmt.Printf("Data by field pull: %s\n", field.Data)
	// fmt.Printf("Data by field index: %s\n", (fileStruct.Fields[0]).Data)

}
