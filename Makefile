all : generate
.PHONY : all

## Generate init.el from README.org
generate :
	@echo "Generate init.el from README.org"
	emacs -Q --batch --find-file "README.org" -f "org-babel-tangle"
.PHONY : generate	
