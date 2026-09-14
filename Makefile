EMACS ?= emacs
ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SITE_LISP := $(ROOT)/site-lisp

INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)
INFO_DIR ?= $(ROOT)/.cache

.PHONY: transient emms

all: magit git-modes emmet-mode expand-region rg wgrep avy mwim symbol-overlay markdown-mode emms dash yasnippet yaml file gptel nov org plantuml-mode posframe prettier rime f s sdcv vertico

clean: compat-clean cond-let-clean llama-clean transient-clean with-editor-clean magit-clean git-modes-clean emmet-mode-clean expand-region-clean rg-clean wgrep-clean avy-clean mwim-clean symbol-overlay-clean markdown-mode-clean emms-clean dash-clean yasnippet-clean yaml-clean file-clean gptel-clean nov-clean org-clean plantuml-mode-clean posframe-clean prettier-clean rime-clean f-clean s-clean sdcv-clean vertico-clean

compat:
	$(MAKE) -C $(SITE_LISP)/compat compile compat.info
	$(INSTALL_INFO) --info-dir=$(INFO_DIR) $(SITE_LISP)/compat/compat.info
compat-clean:
	$(INSTALL_INFO) --delete --quiet --info-dir=$(INFO_DIR) $(SITE_LISP)/compat/compat.info
	$(MAKE) -C $(SITE_LISP)/compat clean

cond-let:
	$(MAKE) -C $(SITE_LISP)/cond-let lisp
cond-let-clean:
	$(MAKE) -C $(SITE_LISP)/cond-let clean

llama: compat
	$(MAKE) -C $(SITE_LISP)/llama lisp
llama-clean:
	$(MAKE) -C $(SITE_LISP)/llama clean

transient: compat cond-let
	$(MAKE) -C $(SITE_LISP)/transient lisp info
transient-clean:
	$(MAKE) -C $(SITE_LISP)/transient clean

with-editor: compat cond-let
	$(MAKE) -C $(SITE_LISP)/with-editor lisp info
with-editor-clean:
	$(MAKE) -C $(SITE_LISP)/with-editor clean

magit: compat cond-let llama transient with-editor
	$(MAKE) -C $(SITE_LISP)/magit/lisp lisp
	$(MAKE) -C $(SITE_LISP)/magit info
magit-clean:
	$(MAKE) -C $(SITE_LISP)/magit clean

git-modes: compat
	$(MAKE) -C $(SITE_LISP)/git-modes lisp
git-modes-clean:
	$(MAKE) -C $(SITE_LISP)/git-modes clean

emmet-mode:
	$(MAKE) -C $(SITE_LISP)/emmet-mode emmet-mode.elc
emmet-mode-clean:
	$(RM) $(SITE_LISP)/emmet-mode/emmet-mode.elc

expand-region:
	$(EMACS) -Q --batch  -L $(SITE_LISP)/expand-region -f batch-byte-compile $(SITE_LISP)/expand-region/*.el
expand-region-clean:
	$(RM) $(SITE_LISP)/expand-region/*.elc

rg: transient wgrep
	$(EMACS) -Q --batch -L $(SITE_LISP)/rg -L $(SITE_LISP)/transient -L $(SITE_LISP)/Emacs-wgrep -f batch-byte-compile $(SITE_LISP)/rg/*.el
	$(INSTALL_INFO) --info-dir=$(INFO_DIR) $(SITE_LISP)/rg/rgel.info
rg-clean:
	$(RM) $(SITE_LISP)/rg/*.elc
	$(INSTALL_INFO) --delete --quiet --info-dir=$(INFO_DIR) $(SITE_LISP)/rg/rgel.info

wgrep:
	$(MAKE) -C $(SITE_LISP)/Emacs-wgrep compile
wgrep-clean:
	$(MAKE) -C $(SITE_LISP)/Emacs-wgrep clean

avy:
	$(MAKE) -C $(SITE_LISP)/avy compile
avy-clean:
	$(MAKE) -C $(SITE_LISP)/avy clean

mwim:
	$(MAKE) -C $(SITE_LISP)/mwim
mwim-clean:
	$(MAKE) -C $(SITE_LISP)/mwim clean

symbol-overlay:
	$(MAKE) -C $(SITE_LISP)/symbol-overlay compile
symbol-overlay-clean:
	rm -f $(SITE_LISP)/symbol-overlay/*.elc

markdown-mode:
	$(MAKE) -C $(SITE_LISP)/markdown-mode
markdown-mode-clean:
	$(MAKE) -C $(SITE_LISP)/markdown-mode clean

emms:
	$(MAKE) -C $(SITE_LISP)/emms
	$(INSTALL_INFO) --info-dir=$(INFO_DIR) $(SITE_LISP)/emms/doc/emms.info
emms-clean:
	$(INSTALL_INFO) --delete --info-dir=$(INFO_DIR) $(SITE_LISP)/emms/doc/emms.info
	$(MAKE) -C $(SITE_LISP)/emms clean

dash:
	$(MAKE) -C $(SITE_LISP)/dash lisp
dash-clean:
	$(MAKE) -C $(SITE_LISP)/dash clean

yasnippet:
	$(MAKE) -C $(SITE_LISP)/yasnippet compile
	$(EMACS) -Q --batch -L $(SITE_LISP)/yasnippet -f batch-byte-compile $(SITE_LISP)/yasnippet-snippets/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/yasnippet -f batch-byte-compile $(SITE_LISP)/auto-yasnippet/auto-yasnippet.el
yasnippet-clean:
	$(MAKE) -C $(SITE_LISP)/yasnippet clean
	$(RM) $(SITE_LISP)/yasnippet-snippets/*.elc
	$(RM) $(SITE_LISP)/auto-yasnippet/*.elc

yaml:
	$(MAKE) -C $(SITE_LISP)/yaml-mode
yaml-clean:
	$(MAKE) -C $(SITE_LISP)/yaml-mode clean

lua:
	$(MAKE) -C $(SITE_LISP)/lua-mode lua-mode.elc
lua-clean:
	$(RM) $(SITE_LISP)/lua-mode/lua-mode.elc

file:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/vimrc-mode/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/nginx-mode/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/tablist -f batch-byte-compile $(SITE_LISP)/tablist/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/pdf-tools/lisp -L $(SITE_LISP)/tablist -f batch-byte-compile $(SITE_LISP)/pdf-tools/lisp/*.el
	$(MAKE) -C $(SITE_LISP)/pdf-tools loaddefs autobuild
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/saveplace-pdf-view/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/graphviz-dot-mode/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/meson-mode/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/crontab-mode/*.el
file-clean:
	$(RM) $(SITE_LISP)/vimrc-mode/*.elc
	$(RM) $(SITE_LISP)/nginx-mode/*.elc
	$(RM) $(SITE_LISP)/tablist/*.elc
	$(MAKE) -C $(SITE_LISP)/pdf-tools clean
	$(RM) $(SITE_LISP)/pdf-tools/lisp/pdf-tools-autoloads.el
	$(RM) $(SITE_LISP)/pdf-tools/lisp/*.elc
	$(RM) $(SITE_LISP)/saveplace-pdf-view/*.elc
	$(RM) $(SITE_LISP)/graphviz-dot-mode/*.elc
	$(RM) $(SITE_LISP)/meson-mode/*.elc
	$(RM) $(SITE_LISP)/crontab-mode/*.elc

gptel: transient compat
	$(EMACS) -Q --batch -L $(SITE_LISP)/transient -L $(SITE_LISP)/compat -L $(SITE_LISP)/gptel -f batch-byte-compile $(SITE_LISP)/gptel/*.el
gptel-clean:
	$(RM) $(SITE_LISP)/gptel/*.elc

nov:
	$(EMACS) -Q --batch -L $(SITE_LISP)/esxml -f batch-byte-compile $(SITE_LISP)/esxml/esxml.el $(SITE_LISP)/esxml/esxml-query.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/esxml -f batch-byte-compile $(SITE_LISP)/nov/*.el
nov-clean:
	$(RM) $(SITE_LISP)/esxml/*.elc
	$(RM) $(SITE_LISP)/nov/*.elc

org:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/gntp/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/alert -L $(SITE_LISP)/gntp -f batch-byte-compile $(SITE_LISP)/alert/alert.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/alert -L $(SITE_LISP)/org-pomodoro -f batch-byte-compile $(SITE_LISP)/org-pomodoro/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/tomelr/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/tomelr -L $(SITE_LISP)/ox-hugo -f batch-byte-compile $(SITE_LISP)/ox-hugo/*.el
org-clean:
	$(RM) $(SITE_LISP)/gntp/*.elc
	$(RM) $(SITE_LISP)/alert/*.elc
	$(RM) $(SITE_LISP)/org-pomodoro/*.elc
	$(RM) $(SITE_LISP)/tomelr/*.elc
	$(RM) $(SITE_LISP)/ox-hugo/*.elc

plantuml-mode:
	$(EMACS) -Q --batch -L $(SITE_LISP)/dash -f batch-byte-compile $(SITE_LISP)/deflate/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/dash -L $(SITE_LISP)/deflate -f batch-byte-compile $(SITE_LISP)/plantuml-mode/*.el
plantuml-mode-clean:
	$(RM) $(SITE_LISP)/deflate/*.elc
	$(RM) $(SITE_LISP)/plantuml-mode/*.elc

posframe:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/posframe/posframe.el
posframe-clean:
	$(RM) $(SITE_LISP)/posframe/*.elc

prettier:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/iter2/iter2.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/s -L $(SITE_LISP)/dash -L $(SITE_LISP)/f -f batch-byte-compile $(SITE_LISP)/nvm/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/iter2 -L $(SITE_LISP)/nvm -L $(SITE_LISP)/s -L $(SITE_LISP)/dash -L $(SITE_LISP)/f -f batch-byte-compile $(SITE_LISP)/prettier/*.el
prettier-clean:
	$(RM) $(SITE_LISP)/iter2/*.elc
	$(RM) $(SITE_LISP)/nvm/*.elc
	$(RM) $(SITE_LISP)/prettier/*.elc

.PHONY: rime
rime:
	$(MAKE) -C $(SITE_LISP)/emacs-rime lib
	$(EMACS) -Q --batch -L $(SITE_LISP)/emacs-rime -L $(SITE_LISP)/dash -L $(SITE_LISP)/posframe -f batch-byte-compile $(SITE_LISP)/emacs-rime/rime*.el
rime-clean:
	$(MAKE) -C $(SITE_LISP)/emacs-rime clean
	$(RM) $(SITE_LISP)/emacs-rime/*.elc

f:
	$(EMACS) -Q --batch -L $(SITE_LISP)/s -L $(SITE_LISP)/dash -f batch-byte-compile $(SITE_LISP)/f/*.el
f-clean:
	$(RM) $(SITE_LISP)/f/*.elc

s:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/s/*.el
s-clean:
	$(RM) $(SITE_LISP)/s/*.elc

sdcv:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/sdcv/*.el
sdcv-clean:
	$(RM) $(SITE_LISP)/sdcv/*.elc

vertico:
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/marginalia/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/vertico/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/vertico -L $(SITE_LISP)/vertico/extensions -f batch-byte-compile $(SITE_LISP)/vertico/extensions/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/corfu/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/corfu -L $(SITE_LISP)/corfu/extensions -f batch-byte-compile $(SITE_LISP)/corfu/extensions/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/orderless -f batch-byte-compile $(SITE_LISP)/orderless/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/cape -f batch-byte-compile $(SITE_LISP)/cape/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/consult -f batch-byte-compile $(SITE_LISP)/consult/*.el
	$(EMACS) -Q --batch -f batch-byte-compile $(SITE_LISP)/emacs-popon/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/corfu -L $(SITE_LISP)/emacs-popon/ -f batch-byte-compile $(SITE_LISP)/emacs-corfu-terminal/*.el
	$(EMACS) -Q --batch -L $(SITE_LISP)/embark -L $(SITE_LISP)/consult -L $(SITE_LISP)/avy -f batch-byte-compile $(SITE_LISP)/embark/*.el
vertico-clean:
	$(RM) $(SITE_LISP)/marginalia/*.elc
	$(RM) $(SITE_LISP)/vertico/*.elc
	$(RM) $(SITE_LISP)/vertico/extensions/*.elc
	$(RM) $(SITE_LISP)/corfu/*.elc
	$(RM) $(SITE_LISP)/corfu/extensions/*.elc
	$(RM) $(SITE_LISP)/orderless/*.elc
	$(RM) $(SITE_LISP)/cape/*.elc
	$(RM) $(SITE_LISP)/consult/*.elc
	$(RM) $(SITE_LISP)/emacs-popon/*.elc
	$(RM) $(SITE_LISP)/emacs-corfu-terminal/*.elc
	$(RM) $(SITE_LISP)/embark/*.elc
