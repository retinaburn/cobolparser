package main

import (
	"os"

	"github.com/retinaburn/cobolparser/parser"
)

func main() {

	//file, err := os.Open("resources/string2.copybook")
	file, err := os.Open("resources/largedecimal.copybook")
	if err != nil {
		panic(err)
	}

	lexer := parser.NewLexer(file)
	fields := parser.ParseLexData(lexer)
	// Data for string2
	//javaData := []int{-63, -62, -128, -127, -126, -125, -124, -123, -122, -121, -120, -119}
	//100.99
	//javaData := []int{-16, -16, -16, -16, -16, -16, -16, -16, -15, -16, -16, -7, -55}
	//-100.99
	javaData := []int{-16, -16, -16, -16, -16, -16, -16, -16, -15, -16, -16, -7, -39}
	parser.ParseData(fields, javaData)

}
