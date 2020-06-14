# panelmap 1.2.1
* Fix bug in `circomap` - where different number of group labes were not getting appropriate colors. 

* `circomap` can now take NAs. Added `NA.flag` and `NA.col` functionality

# panelmap 1.2.0

Introducing panelmap 1.2.0 with following changes - 

* legends are beautiful now, they can be fully customized. You can specify rows and columns in each legend panelet, and your own legend vector 
* introducing 

`makepanel_order`

* Allows you to pass an ordered data without group variable to visualize a panelmap. Returns summary statistics like `median(range)` and `n(%)`
* For visualizing discrete data, specifically mutation data. you can now pas as matrix of `0s` and `1s`, and `makepanel_order` will order it according to row and column sum frequency and plot it! Returns the summary statistics and ordered data matrix. 

# panelmap 1.0.1

Introducing panelmap 1.0.1 with following changes - 

* fix legend spacing
* can pass legend input 

# panelmap 1.0.0

* Added a `NEWS.md` file to track changes to the package.
