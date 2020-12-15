# Stock Dynamics
# Evaluate the companies: IBM, GE, Proctor and Gamble, Coke, Boeing

IBM = read.csv('datasets/IBMStock.csv')
GE = read.csv('datasets/GEStock.csv')
ProcterGamble = read.csv('datasets/ProcterGambleStock.csv')
CocaCola = read.csv('datasets/CocaColaStock.csv')
Boeing = read.csv('datasets/BoeingStock.csv')

str(CocaCola)

# The Date for all these datasets are in string format.
# Convert them to Date format
IBM$Date = as.Date(CocaCola$Date, '%m/%d/%y')
GE$Date = as.Date(CocaCola$Date, '%m/%d/%y')
ProcterGamble$Date = as.Date(CocaCola$Date, '%m/%d/%y')
CocaCola$Date = as.Date(CocaCola$Date, '%m/%d/%y')
Boeing$Date = as.Date(CocaCola$Date, '%m/%d/%y')

min(CocaCola$Date)  # Smallest year
max(CocaCola$Date)  # Largest year
mean(IBM$StockPrice)  # Mean stock price of IBM
min(GE$StockPrice)    # Min stock price of GE
max(CocaCola$StockPrice) # Max stock price of Coke

summary(Boeing)     # We can get the median stock price from summary ...
median(Boeing$StockPrice)  # ... or we can get it using the median function

sd(ProcterGamble$StockPrice) # sd stock price of PG

# Scatter plot of Coke stock prices over time
plot(CocaCola$Date, CocaCola$StockPrice,
     type = 'l',             # type 'l' for line
     xlab = 'Date', ylab = 'Daily Stock Price',
     main = 'Coca Cola stock prices',
     col = 'red')
lines(ProcterGamble$Date, ProcterGamble$StockPrice, 
      lty = 2,               # lty=2 => dashed line
                             # lty=2 => dotted line
                             # lty=4 => alternating dash and dots
                             # lty=5 => long-dashed line
      col = 'blue')
abline(v=as.Date('2000-03-01'), lwd=2)  # vertical line at March 01, 2000 (when stock market dropped)
                                        # v = for vertical line,
                                        # lwd = 2 makes the line thicker
# abline(h=80, lwd=2)                     # h = for horizontal line
                                          # lwd = 2 makes the line thicker
abline(v=as.Date('1983-01-01'), lwd=2)

# To answer which company has lower values over all the dates in the data
mean(CocaCola$StockPrice) > mean(ProcterGamble$Stock)  # Returns FALSE, so Coke has lower values overall

# Stock prices from 1995 through 2005 are the observations numbered from 301 to 432.
# Look at how the stock prices changed for all the 5 companies during this period
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], 
     type='l', col='red', lwd=2,
     ylim=c(0, 210))  # ylim limits the range of the y-axis
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col='blue', lwd=2)
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col='purple', lwd=2)
lines(GE$Date[301:432], GE$StockPrice[301:432], col='orange', lwd=2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col='black', lwd=2)
abline(v=as.Date('2000-03-01'), lwd=2)
abline(v=as.Date('2005-01-01'), lwd=2)
abline(v=as.Date('1997-09-01'), lwd=2, lty=2)
abline(v=as.Date('1997-11-01'), lwd=2, lty=3)

# Find mean stock price per month of IBM
tapply(IBM$StockPrice,    # Step 3. Apply mean to the stock price
       months(IBM$Date),  # Step 1. Group by sorted month values
       mean)              # Step 2. Apply mean on each group
mean(IBM$StockPrice)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)

rm(Boeing, CocaCola, GE, IBM, ProcterGamble)
