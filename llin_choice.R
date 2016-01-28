if (input$llin==T )
{
  myprop0 = cbind(myprop0,
                  LLIN=xts(myprop$mild_value,order.by = semaine)
  )
  a= dygraph( data= myprop0  ,main =  mytitle) 
  for ( j in colnames(myprop0) )
  {
    source("selected_hfi.R",local = T)
    a = a %>% dySeries(j, fillGraph=T,label = mylabel)
  }
}