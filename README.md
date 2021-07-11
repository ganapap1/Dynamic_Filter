# Dynamic_Filter
Dynamic Filter Datatable using SliderInput and SelectInput
Slider Input and Select Input created dynamically from the dataset uploaded to Shiny App, option to save filtered dataset to csv
Filtering datatable using dynamic sliderInput and selectInput
What is dynamic is that the column names for such filters are picked up from the data set which you import into the shiny app
Shiny App will divide numeric and character columns automatically. Numeric columns gets sliderInput with min and max. Character fields takes SelectInput with multiple selection features
You can import any dataset in csv or rds or excel file format
As you move the slider or select single or multiple inputs, dataset gets filtered instantly using reactive function.
There is an interesting option to adjust min and max values of all sliders based on selection in one slider.  To enable this option, you have to tick “Impact ALL Filters” check box
There is an option to download filtered dataset as csv file
Knowledge share & Happy learning



