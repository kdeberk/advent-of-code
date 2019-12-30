package day8

import (
	"strings"
	"utils"
)

const imageWidth = 25
const imageHeight = 6
const layerSize = imageWidth * imageHeight

const (
	black       byte = 0
	white       byte = 1
	transparent byte = 2
)

type layer []byte

func readLayers(filename string) ([]layer, error) {
	line, err := utils.ReadSingleLine(filename)
	if err != nil {
		return nil, err
	}

	pixels := make([]byte, len(line))
	for i, r := range line {
		pixels[i] = byte(r) - '0'
	}

	layers := []layer{}
	for i := 0; i < len(pixels); i += layerSize {
		layers = append(layers, layer(pixels[i:i+layerSize]))
	}
	return layers, err
}

func combineLayers(layers []layer) layer {
	final := make(layer, layerSize)

	// Backwards, because layers are rendered with first layer last.
	for i := len(layers) - 1; 0 <= i; i -= 1 {
		for index, pixel := range layers[i] {
			switch pixel {
			case black:
				final[index] = black
			case white:
				final[index] = white
			case transparent:
			}
		}
	}

	return final
}

func renderLayer(layer layer) string {
	builder := strings.Builder{}

	for row := 0; row < imageHeight; row += 1 {
		for col := 0; col < imageWidth; col += 1 {
			index := row*imageWidth + col

			switch layer[index] {
			case black:
				builder.WriteRune(' ')
			case white:
				builder.WriteRune('#')
			case transparent:
			}
		}

		if row+1 < imageHeight {
			builder.WriteRune('\n')
		}
	}

	return builder.String()
}
