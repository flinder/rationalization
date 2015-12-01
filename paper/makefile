rationalization_manuscript.pdf: rationalization_manuscript.md
	pandoc -H options.sty rationalization_manuscript.md -o rationalization_manuscript.pdf --bibliography=rationalization.bib

rationalization_slides.pdf: rationalization_slides.md
	pandoc -H options_slides.sty -t beamer $< -o $@

