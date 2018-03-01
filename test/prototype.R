# build prototype of functions

library(data.table)
library(magrittr)

# test replacing default data frame with another one ===========================
stat_test <- function(mapping = NULL, data = NULL, geom = "polygon",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, data_replace = NULL, ...){
    layer(
        stat = StatTest, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        # all parameters inside list
        params = list(data_replace = data_replace,
                      na.rm = na.rm, ...)
    )
}


StatTest <- ggproto(
    "StatTest", Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales,
                             params, data_replace){
        names(data_replace) <- c("id", "x", "y")
        data_replace
    }
)


ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
df <- data.frame(
    id = rep(ids, each = 4),
    xx = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    yy = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2),
    value = rep(c(1, NA), each = 4)
)

ggplot(df, aes(x = xx, y = yy, group = id, fill = value)) +
    geom_polygon(fill = NA, color = "red")

ggplot(df, aes(x = xx, y = yy)) +
    geom_polygon(aes(group = id), fill = NA, color = "red") +
    stat_test(data_replace = data_replace, aes(group = ..id..), fill = NA, color = "blue") +
    coord_cartesian(xlim = c(0, 2.7), ylim = c(-1, 3.2))



# fuzzy matching headache ===============
aaa <- function(aaaab = NULL, aaaa = NULL, aaaabc = NULL){
    print(aaaa)
    print(aaaab)
    print(aaaabc)
}

aaa(aa = "aaa")


# test passing ... to another function ========================
# from doc, the seq is
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#     length.out = NULL, along.with = NULL, ...)

# create a function that include by, length.out, along.with in ...
seq_test <- function(from, to, ...){
    2 * seq(from, to, ...)
}

seq_test(1, 10)
    # [1]  2  4  6  8 10 12 14 16 18 20

# new function takes argument by
seq_test(1, 10, by = 2)
    # [1]  2  6 10 14 18
