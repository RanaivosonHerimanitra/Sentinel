######################Variables conversion during importation ####
var_conv= function(df1,df2)
{
  for ( k in names(df1) ) {
    myclass= class(df1[,get(k)])
    if ( myclass!=class(df2[,get(k)]) )
    {
      if (myclass=="integer") { df2[,(k):=as.integer(get(k)),with=F] }
      if (myclass=="numeric") { df2[,(k):=as.numeric(get(k)),with=F] }
      if (myclass=="character") { df2[,(k):=as.character(get(k)),with=F] }
    }
  }
}