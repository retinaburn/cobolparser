package parser

import (
	"bufio"
	"io"
	"unicode"
)

type Token int

const (
	EOF = iota
	ILLEGAL
	IDENT
	INT
	PIC
	TERMINAL
)

var tokens = []string{
	EOF:      "EOF",
	ILLEGAL:  "ILLEGAL",
	IDENT:    "IDENT",
	INT:      "INT",
	PIC:      "PIC",
	TERMINAL: "TERMINAL",
}

func (t Token) String() string {
	return tokens[t]
}

type Position struct {
	line   int
	column int
}

type Lexer struct {
	pos    Position
	reader *bufio.Reader
}

func NewLexer(reader io.Reader) *Lexer {
	return &Lexer{
		pos:    Position{line: 1, column: 0},
		reader: bufio.NewReader(reader),
	}
}

func (l *Lexer) Lex() (Position, Token, string) {
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				return l.pos, EOF, ""
			}

			panic(err)
		}

		l.pos.column++

		switch r {
		case '\n':
			l.resetPosition()
		case '.':
			return l.pos, TERMINAL, "."
		default:
			if unicode.IsSpace(r) {
				continue
			} else if unicode.IsDigit(r) {
				startPos := l.pos
				l.backup()
				lit := l.lexInt()
				return startPos, INT, lit
			} else if isIdentity(r) {
				startPos := l.pos
				l.backup()
				lit := l.lexIdent()

				if lit == "PIC" {
					lit := l.lexPic()
					return startPos, PIC, lit
				}

				return startPos, IDENT, lit
			} else {
				return l.pos, ILLEGAL, string(r)
			}
		}
	}
}

func isIdentity(r rune) bool {
	if unicode.IsLetter(r) {
		return true
	} else if r == rune('-') {
		return true
	} else if unicode.IsDigit(r) {
		return true
	}
	return false
}

func (l *Lexer) resetPosition() {
	l.pos.line++
	l.pos.column = 0
}

func (l *Lexer) backup() {
	if err := l.reader.UnreadRune(); err != nil {
		panic(err)
	}

	l.pos.column--
}

func (l *Lexer) lexInt() string {
	var lit string
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				return lit
			}
		}

		l.pos.column++
		if unicode.IsDigit(r) {
			lit = lit + string(r)
		} else {
			l.backup()
			return lit
		}
	}
}

func (l *Lexer) lexIdent() string {
	var lit string
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				return lit
			}
		}

		l.pos.column++
		if isIdentity(r) {
			lit = lit + string(r)
		} else {
			l.backup()
			return lit
		}
	}
}

func (l *Lexer) lexPic() string {
	var lit = "PIC"
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				return lit
			}
		}
		l.pos.column++
		if unicode.IsLetter(r) ||
			unicode.IsDigit(r) ||
			r == rune('(') ||
			r == rune(')') ||
			unicode.IsSpace(r) ||
			r == rune('-') {
			lit = lit + string(r)
		} else {
			l.backup()
			return lit
		}
	}
}
