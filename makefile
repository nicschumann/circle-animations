COMPILE-SUFFIX = -compiled

MAIN = Main.elm
JS-TARGET = Main$(COMPILE-SUFFIX).js
HTML-TARGET = index$(COMPILE-SUFFIX).html

OUTPUT-DIR = output


all: build

build: build-js

serve: build serve-start

clean: serve-stop clean-files


build-js:
	elm-make $(MAIN) --output=$(OUTPUT-DIR)/$(JS-TARGET)

build-html:
	elm-make $(MAIN) --output=$(OUTPUT-DIR/$(HTML-TARGET)


serve-start:
	livereload $(OUTPUT-DIR) &
	http-server $(OUTPUT-DIR) &

serve-stop:
	kill -9 $$(ps aux | grep -v grep | grep "livereload" | awk '{print $$2}') 
	kill -9 $$(ps aux | grep -v grep | grep "http-server" | awk '{print $$2}') 

# Clean up after yourself
clean-files:
	rm -f $(OUTPUT-DIR)/*$(COMPILE-SUFFIX).js
	rm -f $(OUTPUT-DIR)/*$(COMPILE-SUFFIX).html
	rm -f $(OUTPUT-DIR)/style/*$(COMPILE-SUFFIX).css

# Install development dependencies, if needed
dev-dependencies:
	npm install -g livereload
	npm install -g http-server