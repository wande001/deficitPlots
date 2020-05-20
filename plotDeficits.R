computeDeficitLine <- function(dates, var){
  years = unique(as.numeric(format(dates, "%Y")))
  year = 2019
  startDay = as.numeric(format(as.Date(sprintf("%d-04-01", year)), "%j"))
  lastDay = as.numeric(format(as.Date(sprintf("%d-09-30", year)), "%j"))
  sel = which(as.numeric(format(dates, "%Y")) == year & as.numeric(format(dates, "%j")) <= as.numeric(lastDay) & as.numeric(format(dates, "%j")) >= startDay)
  output = array(NA, c(length(years), length(sel)))
  for(year in years){
    startDay = as.numeric(format(as.Date(sprintf("%d-04-01", year)), "%j"))
    lastDay = as.numeric(format(as.Date(sprintf("%d-09-30", year)), "%j"))
    if(year == tail(years, 1)){lastDay = tail(format(dates, "%j"),1)}
    sel = which(as.numeric(format(dates, "%Y")) == year & as.numeric(format(dates, "%j")) <= as.numeric(lastDay) & as.numeric(format(dates, "%j")) >= startDay)
    if(length(sel) >0){
      output[year-years[1]+1,1] = max(0,var[sel][1])
      for(i in 2:length(sel)){
        output[year-years[1]+1,i] = max(0,output[year-years[1]+1,i-1] + var[sel][i])
      }
    }
  }
  return(output)
}

metData = read.table("etmgeg_260.txt", skip=47, sep=",", header=T)

metDates = as.POSIXlt(as.character(metData[,2]), tz="GMT", "%Y%m%d")
metYears = format(metDates, "%Y")
P = metData[,23]/10
P[P < 0] = 0
E = metData[,41]/10
E[E < 0] = 0

defToday = computeDeficitLine(metDates, E-P)

png("NeerslagTekort_Bilt_actueel.png", width=1500, height=700, pointsize = 28)
plot(1,1, type="n", xlim=c(90,272), ylim=c(0,400), xaxs="i", yaxs="i", xlab="", main="Neerslagtekort de Bilt (mm)", axes="FALSE", ylab="")
abline(h = seq(0,1000,100), col="lightgrey")
axis(2)
axis(1, c(0,31,59,90, 120, 151, 181, 212, 243, 273,304,334,365), labels=rep("",13))
box()
mtext(c("jan","feb","","apr","mei", "jun","jul", "aug", "sep","","nov","dec"), 1, at=c(15, 43, 74, 105, 135, 166, 197, 227, 258,288,318,348), cex=1.0, line=1.1)

for(y in 1:120){
  lines(90:272, defToday[y,], col="grey", lwd=1)
}

lines(90:272, defToday[120,], col="#7570b3", lwd=4)
lines(90:272, defToday[118,], col="#d95f02", lwd=4)
lines(90:272, defToday[76,], col='#1b9e77', lwd=4)
lines(90:272, colMeans(defToday, na.rm=T), col=1, lwd=4)

#legend("topright", c("1.5% droogste jaren (1 keer per 50 jaar)", "5% droogste jaren (1 keer per 20 jaar)", "Normaal", paste("Jaar 2018: actueel", round(tail(Q,1)), "m3/s")), col=c("Red", "Green","Blue", "Black"), lwd=4)
legend("topleft", c("Normaal","1976", "2018", "2020"),  lwd=4, col=c(1, '#1b9e77','#d95f02','#7570b3'))
mtext(paste("(c) Niko Wanders - Universiteit Utrecht, Data KNMI, laatste gegevens van", format(tail(metDates,1),"%d-%m-%Y")),1, at=90, cex=0.8, line=2.5, adj=0)
dev.off()
