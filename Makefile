# tools for active package development

check:
	Rscript -e "devtools::check()"

gui_dev:
	bundle exec guard
