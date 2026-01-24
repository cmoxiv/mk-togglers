# Makefile for mk-togglers

# Programs
EMACS = emacs
MAKEINFO = makeinfo
INSTALL_INFO = install-info
TEXI2PDF = texi2pdf

# Directories
PREFIX = /usr/local
INFODIR = $(PREFIX)/share/info
LISPDIR = $(PREFIX)/share/emacs/site-lisp/mk-togglers

# Files
TEXIFILE = mk-togglers.texi
INFOFILE = mk-togglers.info
ELFILE = mk-togglers.el
ELCFILE = mk-togglers.elc
PDFFILE = mk-togglers.pdf

.PHONY: all clean install uninstall info pdf compile check

# Default target
all: info compile

# Build Info documentation
info: $(INFOFILE)

$(INFOFILE): $(TEXIFILE)
	$(MAKEINFO) $(TEXIFILE)

# Build PDF documentation
pdf: $(PDFFILE)

$(PDFFILE): $(TEXIFILE)
	$(TEXI2PDF) $(TEXIFILE)

# Byte-compile Emacs Lisp
compile: $(ELCFILE)

$(ELCFILE): $(ELFILE)
	$(EMACS) -batch -f batch-byte-compile $(ELFILE)

# Check for warnings
check:
	$(EMACS) -batch -f batch-byte-compile $(ELFILE)

# Install Info documentation and Emacs Lisp
install: all
	@echo "Installing Info documentation to $(INFODIR)..."
	install -d $(INFODIR)
	install -m 644 $(INFOFILE) $(INFODIR)/$(INFOFILE)
	$(INSTALL_INFO) --info-dir=$(INFODIR) $(INFODIR)/$(INFOFILE)
	@echo "Installing Emacs Lisp to $(LISPDIR)..."
	install -d $(LISPDIR)
	install -m 644 $(ELFILE) $(LISPDIR)/$(ELFILE)
	install -m 644 $(ELCFILE) $(LISPDIR)/$(ELCFILE)
	@echo "Installation complete!"
	@echo "Add the following to your Emacs init file:"
	@echo "  (add-to-list 'load-path \"$(LISPDIR)\")"
	@echo "  (require 'mk-togglers)"

# Uninstall
uninstall:
	@echo "Removing Info documentation..."
	$(INSTALL_INFO) --delete --info-dir=$(INFODIR) $(INFODIR)/$(INFOFILE)
	rm -f $(INFODIR)/$(INFOFILE)
	@echo "Removing Emacs Lisp..."
	rm -rf $(LISPDIR)
	@echo "Uninstallation complete!"

# Clean build artifacts
clean:
	rm -f $(INFOFILE) $(ELCFILE) $(PDFFILE)
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

# Help
help:
	@echo "mk-togglers Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Build Info documentation and compile Emacs Lisp (default)"
	@echo "  info       - Build Info documentation (.info file)"
	@echo "  pdf        - Build PDF documentation"
	@echo "  compile    - Byte-compile Emacs Lisp"
	@echo "  check      - Check for compilation warnings"
	@echo "  install    - Install Info docs and Emacs Lisp to PREFIX ($(PREFIX))"
	@echo "  uninstall  - Remove installed files"
	@echo "  clean      - Remove build artifacts"
	@echo "  help       - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX     - Installation prefix (default: /usr/local)"
	@echo "  INFODIR    - Info directory (default: PREFIX/share/info)"
	@echo "  LISPDIR    - Emacs Lisp directory (default: PREFIX/share/emacs/site-lisp/mk-togglers)"
	@echo ""
	@echo "Examples:"
	@echo "  make                    # Build everything"
	@echo "  make info               # Build Info documentation only"
	@echo "  make pdf                # Build PDF manual"
	@echo "  make PREFIX=~/.local install  # Install to ~/.local"
