cg: clean_grammar

clean_grammar:
	pwd
	@rm -f grammar/*.aux grammar/*.bbl grammar/*.bcf grammar/*.blg grammar/*.dvi \ 
		grammar/*.fdb_latexmk grammar/*.fls grammar/*.log grammar/*.pdf \ 
		grammar/*.run.xml grammar/*.synctex.gz grammar/*.fls
