data = read.csv('Capture_first.csv', header=TRUE, sep = ';')
ADNI2 <- subset_function_keep(data, 'Phase', 'ADNI 2')

by(ADNI2$Age, list(ADNI2$Distractor), stat.desc)

write.csv(MyData, file = "MyData.csv")