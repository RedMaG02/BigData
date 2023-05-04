men_moment <- read.csv("lab4_parus_m.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)
women_moment <-read.csv("lab4_parus_f.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)


men_places <- sapply(men_moment[,-1], sum)
women_places <- sapply(women_moment[,-1], sum)

par(mfrow=c(1,2))
barplot(men_places, names=c(1:8), col="purple", xlab="Место", ylab="Количество", main="Мужчины (1996 - 2020)")
barplot(women_places, names=c(1:8), col="purple", xlab="Место", ylab="Количество", main="Женщины (1996 - 2020)")


men_first_place <- men_moment[,c(1:2)][men_moment$X1 > 0, ]
women_first_place <- women_moment[,c(1:2)][women_moment$X1 > 0, ]

pie(men_first_place$X1, labels=men_first_place$X1, col=rainbow(length(men_first_place$X1)), main = "Количество золотых медалей (мужчины)\n(1996 - 2020)")
legend(-1.1, 1.1, men_first_place$Год, cex = 0.7, fill=rainbow(length(men_first_place$Год)))

pie(women_first_place$X1, labels=women_first_place$X1, col=rainbow(length(women_first_place$X1)), main = "Количество золотых медалей (женщины)\n(1996 - 2020)")
legend(-1, 1, women_first_place$Год, cex = 0.7, fill=rainbow(length(women_first_place$Год)))


men_prize <- data.frame(Год=men_moment$Год, Призовых=rowSums(men_moment[, 2:4]))
women_prize <- data.frame(Год=women_moment$Год, Призовых=rowSums(women_moment[, 2:4]))

par(mfrow=c(1,1))
plot(men_prize, type="b", pch=19, col="blue", xaxt="n", ylim=c(0,7), main="Призовые места Бразилия + Германия по футболу за последние 30 лет (1996 - 2020)")
lines(women_prize, type="o", pch=19, col="pink")
legend(min(men_moment$Год), 7, c("Мужчины", "Женщины"), fill=c("blue", "pink"))
axis(side=1, at=men_prize$Год)



events_gold <- read.csv("gold.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

plot(events_gold$Год, events_gold$США, type="b", pch=19, col="red", xaxt="n", ylim=c(0,50), xlab="Год", ylab="Золотых медалей", main="Золотые медали за 6 последних летних олимпиад (2012 - 2022)")
lines(events_gold$Год, events_gold$Китай, type="o", pch=19, col="orange")
lines(events_gold$Год, events_gold$Япония, type="o", pch=19, col="yellow")
lines(events_gold$Год, events_gold$Великобритания, type="o", pch=19, col="green")
lines(events_gold$Год, events_gold$Россия, type="o", pch=19, col="lightblue")
lines(events_gold$Год, events_gold$Украина, type="o", pch=19, col="blue")
lines(events_gold$Год, events_gold$КНДР, type="o", pch=19, col="purple")
axis(side=1, at=events_gold$Год)
legend(max(events_gold$Год) - 1.5, 53, c("США", "Китай", "Япония", "Великобритания", "Россия", "Украина", "КНДР"), fill=c("red", "orange", "yellow", "green", "lightblue", "blue", "purple"))


events_prizes <- read.csv("priz.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

plot(events_prizes$Год, events_prizes$США, type="b", pch=19, col="red", xaxt="n", ylim=c(0,130), xlab="Год", ylab="Медалей", main="Призовые медали за 6 последних летних олимпиад (2012 - 2022)")
lines(events_prizes$Год, events_prizes$КИТАЙ, type="o", pch=19, col="orange")
lines(events_prizes$Год, events_prizes$Япония, type="o", pch=19, col="yellow")
lines(events_prizes$Год, events_prizes$Великобритания, type="o", pch=19, col="green")
lines(events_prizes$Год, events_prizes$Россия, type="o", pch=19, col="lightblue")
lines(events_prizes$Год, events_prizes$Украина, type="o", pch=19, col="blue")
lines(events_prizes$Год, events_prizes$КНДР, type="o", pch=19, col="purple")
axis(side=1, at=events_prizes$Год)
legend(max(events_prizes$Год) - 1, 137, c("США", "Китай", "Япония", "Великобритания", "Россия", "Украина", "КНДР"), fill=c("red", "orange", "yellow", "green", "lightblue", "blue", "purple"))


men_prize_6_ol <- tail(men_prize, 6)
women_prize_6_ol <- tail(women_prize, 6)

par(mfrow=c(1,3))
plot(men_prize_6_ol, type="b", pch=19, col="blue", xaxt="n", ylim=c(0,7), main="Призовые места Бразилия + Германия по футболу\nза последние 6 ОИ (2000 - 2020)")
lines(women_prize_6_ol, type="o", pch=11, col="pink")
legend(min(women_prize_6_ol$Год), 7.2, cex=0.7 ,c("Мужчины", "Женщины"), fill=c("blue", "pink"))
axis(side=1, at=men_prize$Год)

group_prize = data.frame(Призовых_М=men_prize_6_ol$Призовых, Призовых_Ж=women_prize_6_ol$Призовых)
barplot(height=t(as.matrix(group_prize)), beside=TRUE, xlab="Год", ylab="Количество", names.arg=women_prize_6_ol$Год, col=c("blue", "pink"), main="Количество Призовые места Бразилия + Германия по футболу\nза последние 6 ОИ (2000 - 2020)")

prize_6_sum <- sapply(group_prize, sum)
pie(prize_6_sum, labels=c(prize_6_sum["Призовых_Муж"], prize_6_sum["Призовых_Жен"]), col=c("blue", "pink"), main="Всего призовых мест у М и Ж из Бразилия + Германия\nпо футболу за последние 6 ОИ (2000 - 2020)")
