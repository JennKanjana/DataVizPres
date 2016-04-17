BS <- filter(tweet_all, grepl("Bernie|Sanders|BernieSanders", text, ignore.case=TRUE))
HC <- filter(tweet_all, grepl("Hillary|Clinton|Hillaryclinton", text, ignore.case=TRUE))
TC <- filter(tweet_all, grepl("Ted|Cruz|Tedcruz", text, ignore.case=TRUE))
DT <- filter(tweet_all, grepl("Donald|Trump|Donaldtrump", text, ignore.case=TRUE))