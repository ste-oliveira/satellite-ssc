getRegressorsAll <- function(){
        return (c('B1', 'B2', 'B3', 'B4', 'B5', # raw bands
                  'B1.2', 'B2.2', 'B3.2', 'B4.2', 'B5.2', # squared bands
                  'site_no', # no clear way to add categorical variables
                  'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1', # band ratios
                  'B3.B2', 'B4.B2', 'B5.B2',
                  'B4.B3', 'B5.B3', 'B5.B4'))
}

getRegressorsNoSite <- function(){
       return(c('B1', 'B2', 'B3', 'B4', 'B5', # raw bands
          'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1' , # band ratios
          'B3.B2', 'B4.B2', 'B5.B2', 'B4.B3', 'B5.B3', 'B5.B4'))
}
 