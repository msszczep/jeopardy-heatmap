compile:
	rm jeopardyheatmap.html
	elm make --optimize --output=jeopardyheatmap.html src/Main.elm

format:
	elm-format src/Main.elm --yes
