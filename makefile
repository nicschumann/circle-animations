COMPILE-SUFFIX = -compiled

MAIN = Main.elm
STYLE-DIR = style
JS-TARGET = Main$(COMPILE-SUFFIX).js
HTML-TARGET = index$(COMPILE-SUFFIX).html

OUTPUT-DIR = output


all: build

build: build-js

serve: build serve-start

stop: serve-stop clean

open:
	open http://localhost:8080


build-js:
	elm-make $(MAIN) --output=$(OUTPUT-DIR)/$(JS-TARGET)

build-html:
	elm-make $(MAIN) --output=$(OUTPUT-DIR/$(HTML-TARGET)


serve-start:
	livereload $(OUTPUT-DIR) &
	http-server $(OUTPUT-DIR) &
	sass --watch $(STYLE-DIR)/main.scss:$(OUTPUT-DIR)/$(STYLE-DIR)/main$(COMPILE-SUFFIX).css &

serve-stop:
	kill -9 $$(ps aux | grep -v grep | grep "livereload" | awk '{print $$2}') 
	kill -9 $$(ps aux | grep -v grep | grep "http-server" | awk '{print $$2}') 
	kill -9 $$(ps aux | grep -v grep | grep "sass" | awk '{print $$2}') 

# Clean up after yourself
clean:
	rm -f $(OUTPUT-DIR)/*$(COMPILE-SUFFIX).js
	rm -f $(OUTPUT-DIR)/*$(COMPILE-SUFFIX).html
	rm -f $(OUTPUT-DIR)/style/*$(COMPILE-SUFFIX).css*

# Install development dependencies, if needed
dev-dependencies:
	npm install -g livereload
	npm install -g http-server
	npm install -g sass