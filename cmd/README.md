    
Parse the copybook voa the lexer:

    file, err := os.Open("resources/largedecimal.copybook")
	if err != nil {
		panic(err)
	}

	lexer := parser.NewLexer(file)
```
1:8	|INT|	|01|
1:12	|IDENT|	|CRAIG-AREA|
1:22	|TERMINAL|	|.|
2:12	|INT|	|04|
2:16	|IDENT|	|CRAIG-DATA|
2:26	|TERMINAL|	|.|
3:16	|INT|	|05|
3:20	|IDENT|	|AVAIL-BAL|
3:33	|PIC|	|PIC S9(11)V9(2) COMP-3|
3:55	|TERMINAL|	|.|
```

Get the fields from the lexed copybook:

    fields := parser.ParseLexData(lexer)


Parse the data stream into the defined fields:

    javaData := []int{-16, -16, -16, -16, -16, -16, -16, -16, -15, -16, -16, -7, -39}
	parser.ParseData(fields, javaData)
```
[240 240 240 240 240 240 240 240 241 240 240 249 217] -> -00000000100.99 -> -100.99
Process 54488 has exited with status 0
Detaching
```