
project=ttalk
pkgs=-pkgs js_of_ocaml.log,js_of_ocaml,base64,js_of_ocaml.syntax -syntax camlp4o
ocamlbuild_flags=-cflag -annot -use-ocamlfind
JSO_FLAGS := +weak.js

ifeq ($(debug),)
FLAGS :=  $(ocamlbuild_flags) $(pkgs)
else
# e.g. `make debug=1 dev'
FLAGS :=  $(ocamlbuild_flags) -lflag -g -cflag -g $(pkgs)
JSO_FLAGS += --enable excwrap --debuginfo --pretty --noinline
endif

all: js
	rm -rf ~/Downloads/ttalk
	mkdir -p ~/Downloads/ttalk
	cp ttalk.css ttalk.js ttalk.html ~/Downloads/ttalk

js:
	ocamlbuild $(FLAGS) $(project).byte
	js_of_ocaml $(JSO_FLAGS) $(project).byte

