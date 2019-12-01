package line_reader

import (
	"bufio"
	"os"
	"strings"
)

type LineReader struct {
	reader   *bufio.Reader
	nextLine *string
	err      error
}

func NewLineReader(filename string) (*LineReader, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	return &LineReader{bufio.NewReader(file), nil, nil}, nil
}

func (self *LineReader) HasLine() bool {
	if self.nextLine == nil {
		var nextLine string
		nextLine, self.err = self.reader.ReadString('\n')
		self.nextLine = &nextLine
	}
	return self.err == nil
}

func (self *LineReader) ReadLine() string {
	var line string

	if self.HasLine() {
		line = strings.TrimSuffix(*self.nextLine, "\n")
		self.nextLine = nil
	}
	return line
}
