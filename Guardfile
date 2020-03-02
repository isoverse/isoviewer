# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

data = 'inst/extdata'
port = 5000

#guard 'process', name: 'Shiny', command: ['R', '-e', "devtools::load_all('.'); isoviewer:::turn_debug_on(); isoviewer::run(data_dir = '#{data}', allow_data_upload = TRUE, allow_folder_creation = TRUE, port = #{port})"] do
#  watch(%r{NAMESPACE})
#  watch(%r{R/.+\.R$})
#end

guard 'process', name: 'Shiny', command: ['R', '-e', "
  devtools::load_all('.');
  if (FALSE) {
  cfx <- isoreader::iso_read_continuous_flow('inst/extdata/datasets/gc_irms_example_carbon.cf.rds');
  isoreader::iso_save(cfx, 'inst/extdata/datasets/gc_irms_example_carbon.cf.rds');
  dix <- isoreader::iso_read_dual_inlet('inst/extdata/datasets/dual_inlet_examples.di.rds');
  dix <- isoreader::iso_reread_files(dix);
  isoreader::iso_save(dix, 'inst/extdata/datasets/dual_inlet_examples.di.rds');
  scnx <- isoreader::iso_read_scan('inst/extdata/datasets/scan_examples.scan.rds', read_cache = FALSE);
  scnx <- isoreader::iso_reread_files(scnx);
  isoreader::iso_save(scnx, 'inst/extdata/datasets/scan_examples.scan.rds');
  } else {
  cfx <- readr::read_rds('inst/extdata/datasets/gc_irms_example_carbon.cf.rds');
  dix <- readr::read_rds('inst/extdata/datasets/dual_inlet_examples.di.rds');
  scnx <- readr::read_rds('inst/extdata/datasets/scan_examples.scan.rds');
  };
  cfx_wpt <- isoprocessor::iso_set_peak_table_automatically_from_vendor_data_table(cfx[1:10]);
  if (file.exists('gui_settings.rds')) isoviewer:::load_gui_settings('gui_settings.rds');
  isoviewer:::turn_debug_on();
  isoviewer::iso_start_viewer(log = TRUE, runApp_params = list(port = #{port}))"] do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end

guard 'livereload', grace_period: 3 do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end
