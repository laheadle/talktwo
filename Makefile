
project=talktwo
pkgs=-pkgs js_of_ocaml.log,js_of_ocaml,base64,js_of_ocaml.syntax -syntax camlp4o
ocamlbuild_flags=-cflag -annot -use-ocamlfind
JSO_FLAGS := +weak.js
outputs=talktwo.css talktwo.js talktwo.html jsdiff_native.js
diffoutputs=jsdiff.js diff.js web_example.html

ifeq ($(debug),)
FLAGS :=  $(ocamlbuild_flags) $(pkgs)
else
# e.g. `make debug=1 dev'
FLAGS :=  $(ocamlbuild_flags) -lflag -g -cflag -g $(pkgs)
JSO_FLAGS += --enable excwrap --debuginfo --pretty --noinline
endif

all: js
	rm -rf ~/Downloads/talktwo
	mkdir -p ~/Downloads/talktwo
	cp $(outputs) ~/Downloads/talktwo

js:
	ocamlbuild $(FLAGS) $(project).byte
	js_of_ocaml $(JSO_FLAGS) $(project).byte

upload:
	./upload.sh $(outputs)

difftest:
	ocamlbuild $(FLAGS) diff.byte
	js_of_ocaml $(JSO_FLAGS) diff.byte
	rm -rf ~/Downloads/talktwo
	mkdir -p ~/Downloads/talktwo
	cp $(diffoutputs) ~/Downloads/talktwo

