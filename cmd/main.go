package main

import (
	"fmt"
	"os"

	"github.com/retinaburn/cobolparser/parser"
)

func main() {

	//file, err := os.Open("resources/string2.copybook")
	//file, err := os.Open("resources/largedecimal.copybook")
	//file, err := os.Open("resources/unsigned-binary.copybook")
	//file, err := os.Open("resources/unsigned-binary.copybook")
	//file, err := os.Open("resources/float.copybook")
	//file, err := os.Open("resources/alpha.copybook")
	file, err := os.Open("resources/number.copybook")
	if err != nil {
		panic(err)
	}

	lexer := parser.NewLexer(file)
	fileStruct := parser.ParseLexData(lexer)
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
	fmt.Printf("Record length: %d\n", fileStruct.RecordLength)
	javaData := []int{-15, -14, -13, -12, -11, -10, -9, -8, -7, -15, -14, -13}
	parser.ParseData(&fileStruct, javaData)
	fmt.Printf("Current read data: %d\n", fileStruct.StartPos)

	parser.ParseData(&fileStruct, javaData)
	fmt.Printf("Current read data: %d\n", fileStruct.StartPos)

}
