k <- k+1
df$abundance <- abundances(x)[cm[[k]],]
p <- ggplot(df,
         aes(x = days_since_experiment_start, y = abundance)) +
       geom_smooth() +
       geom_point()

print(p)