BS <- filter(tweet_all, grepl("Bernie|Sanders|BernieSanders|Feelthebern|berniebro", text, ignore.case=TRUE))
HC <- filter(tweet_all, grepl("Hillary|Clinton|Hillaryclinton|HRC|withher|Hil", text, ignore.case=TRUE))
TC <- filter(tweet_all, grepl("Ted|Cruz|Tedcruz", text, ignore.case=TRUE))
DT <- filter(tweet_all, grepl("Donald|Trump|Donaldtrump|Drumpf", text, ignore.case=TRUE))
