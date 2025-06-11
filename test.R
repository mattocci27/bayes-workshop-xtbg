library(tidyverse)
library(ggplot2)
library(gganimate)
library(patchwork)

hist(rnorm(100))

# 1. Dummy data
df <- tibble(
  x     = seq(0, 2*pi, length.out = 50),
  y1    = sin(x),
  y2    = cos(x),
  frame = row_number()
)

# 2. Build each panel **with** its own transition
p1_anim <- ggplot(df, aes(x, y1)) +
  geom_line() +
  geom_point(aes(frame = frame), colour = "blue", size = 3) +
  labs(title = "Sine — frame {current_frame}") +
  theme_minimal() +
  transition_manual(frames = frame) +
  ease_aes("linear")

p2_anim <- ggplot(df, aes(x, y2)) +
  geom_line() +
  geom_point(aes(frame = frame), colour = "red", size = 3) +
  labs(title = "Cosine — frame {current_frame}") +
  theme_minimal() +
  transition_manual(frames = frame) +
  ease_aes("linear")

# 3. Now combine them
p_combined <- (p1_anim / p2_anim)

# 4. Render
animate(p_combined,
        nframes  = max(df$frame),
        fps      = 10,
        width    = 600,
        height   = 600,
        renderer = gifski_renderer("two_panels.gif"))



Or simply assing the 1st row of theta_start p_start to the last row.
Then, we can keep the number of rows the same as the original data frame.

[ins] r$>  one_step %>%
              as_tibble() %>%
              # retain your original gr3 logic
              mutate(gr3 = tau) %>%
              # now create two new columns, only non‐NA at row 1 and row n
              mutate(
                theta_start = if_else(row_number() == 1, theta, NA_real_),
                p_start     = if_else(row_number() == 1, p,     NA_real_),
                theta_end   = if_else(row_number() == n(), theta, NA_real_),
                p_end       = if_else(row_number() == n(), p,     NA_real_)
              )
# A tibble: 8 × 10
   temp        p theta hamiltonian    gr   gr3 theta_start p_start theta_end  p_end
  <dbl>    <dbl> <dbl>       <dbl> <int> <int>       <dbl>   <dbl>     <dbl>  <dbl>
1     1 -0.635   0.774        12.8    15    15       0.774  -0.635    NA     NA
2     1 -0.563   0.700        12.8    15    15      NA      NA        NA     NA
3     1 -0.331   0.645        12.8    15    15      NA      NA        NA     NA
4     1  0.00921 0.625        12.8    15    15      NA      NA        NA     NA
5     1  0.346   0.647        12.8    15    15      NA      NA        NA     NA
6     1  0.571   0.703        12.8    15    15      NA      NA        NA     NA
7     1  0.634   0.777        12.8    15    15      NA      NA        NA     NA
8     1  0.548   0.850        12.8    15    15      NA      NA         0.850  0.548


set.seed(123)
theta   <- 2.5
eps     <- 0.01
T       <- 15
scale_p <- 1
prev_p  <- 0
L       <- 96

# initialize
step  <- NULL
step2 <- tibble()

step  <- tibble(temp = double(),
                p    = double(),
                prev_p = double(),
                hamiltonian = double(),
                gr   = integer())
for (tau in 1:T) {
  # 1a) draw a fresh momentum
  p <- rnorm(1, 0, scale_p)
  # 1b) record the “kick” (you had this as temp=2 row)
  step <- rbind(step,
                c(temp=2, p, prev_p, hamiltonian=0, gr=tau, gr2=1))

  # 2) run leapfrog
  one_step <- move_one_step(theta, shape, rate,
                            p, eps=eps, L=L, stlide=12) %>%
    as_tibble() %>%
    mutate(gr  = tau,
           gr2 = if_else(row_number()==n(), 1L, 2L))

  # 3) build one_step2: keep all rows, but only fill start/end
  # one_step2 <- one_step %>%
  #   mutate(
  #     gr3         = tau,
  #     theta_start = NA_real_,
  #     p_start     = NA_real_,
  #     theta_end   = if_else(row_number()==n(), theta,      NA_real_),
  #     p_end       = if_else(row_number()==n(), p,          NA_real_)
  #   )

  # theta_start <- one_step$theta[1]  # first row theta
  # p_start <- one_step$p[1]  # first row theta
  # one_step2$theta_start[nrow(one_step)] <- theta_start
  # one_step2$p_start[nrow(one_step)] <- p_start

  # 4) append
  step  <- bind_rows(step,  one_step)
  # step2 <- bind_rows(step2, one_step2)

  # 5) update for next iteration
  theta   <- one_step$theta[nrow(one_step)]
  prev_p  <- one_step$p    [nrow(one_step)]
}

step_anim  <- step2 %>%
  mutate(time = row_number())   # blue points appear cumulatively

# 6) make them tibbles and factor where needed
step  <- as_tibble(step)  %>%
  filter(temp != 2) %>%        # drop those artificial rows
  mutate(gr = factor(gr))
# step2 <- as_tibble(step2) %>%
#   mutate(gr3 = as.integer(gr3))  # numeric for frame


# --- 2) Prepare for animation ---------------------------------------------

step_anim  <- step2 %>%
  mutate(time = row_number())   # blue points appear cumulatively

step2_anim <- step2 %>%
  mutate(time = gr3)            # red segment only shows at gr3



8 th row: make a new col for p start, theta_start from 8th row, p_end, theta_end from 9th row.
Does it make sense?
We repeat this for each gr (leapfrog).


[ins] r$> as.data.frame(step_anim)
    temp            p prev_p hamiltonian gr     theta
1      1 -1.629512216     NA    23.49411  1 2.3683315
2      1 -2.655500561     NA    23.49398  1 2.1106072
3      1 -3.592322000     NA    23.49369  1 1.7344986
4      1 -4.346642951     NA    23.49293  1 1.2555237
5      1 -4.654746154     NA    23.49023  1 0.7081886
6      1 -3.121151578     NA    23.47942  1 0.2023360
7      1  3.605257146     NA    23.47998  1 0.2549572
8      1  4.661787115     NA    23.49085  1 0.7806009
9      1 -0.224671538     NA    12.65122  2 0.7527307
10     1 -0.163373383     NA    12.65122  2 0.7289173
11     1 -0.059065605     NA    12.65123  2 0.7152500
12     1  0.061330101     NA    12.65123  2 0.7153892
13     1  0.165026911     NA    12.65122  2 0.7292970
14     1  0.225319614     NA    12.65122  2 0.7532509
15     1  0.229729788     NA    12.65122  2 0.7811327
16     1  0.180848676     NA    12.65122  2 0.8062541
17     1  1.341271281     NA    13.84984  3 0.9829556
18     1  0.919559201     NA    13.84998  3 1.1201148
19     1  0.389987653     NA    13.85004  3 1.1994029
20     1 -0.179439285     NA    13.85005  3 1.2121420
21     1 -0.730915162     NA    13.85001  3 1.1570446
22     1 -1.202076843     NA    13.84990  3 1.0398852
23     1 -1.509587721     NA    13.84972  3 0.8749893
24     1 -1.528006640     NA    13.84956  3 0.6889870
25     1  0.233008074     NA    12.68463  4 0.7076676
26     1  0.330767334     NA    12.68462  4 0.7423008
27     1  0.344205820     NA    12.68462  4 0.7836678
28     1  0.278217991     NA    12.68463  4 0.8217341
29     1  0.153473597     NA    12.68464  4 0.8480965
30     1 -0.001805942     NA    12.68464  4 0.8573516
31     1 -0.156733502     NA    12.68464  4 0.8476771
32     1 -0.280448055     NA    12.68463  4 0.8209788
33     1  0.020949235     NA    12.65366  5 0.8301572
34     1 -0.091726703     NA    12.65366  5 0.8258336
35     1 -0.185223336     NA    12.65366  5 0.8089077
36     1 -0.238456323     NA    12.65365  5 0.7829877
37     1 -0.236613214     NA    12.65365  5 0.7538813
38     1 -0.176172989     NA    12.65366  5 0.7285502
39     1 -0.069308589     NA    12.65366  5 0.7134660
40     1  0.056519400     NA    12.65367  5 0.7126798
41     1  1.633823346     NA    14.12302  6 0.9174085
42     1  1.266058450     NA    14.12322  6 1.0935270
43     1  0.741928278     NA    14.12333  6 1.2151145
44     1  0.143741347     NA    14.12338  6 1.2686756
45     1 -0.467592411     NA    14.12336  6 1.2490948
46     1 -1.034789217     NA    14.12328  6 1.1581907
47     1 -1.488169193     NA    14.12313  6 1.0052222
48     1 -1.723922032     NA    14.12289  6 0.8095855
49     1  0.338099950     NA    12.74318  7 0.8583819
50     1  0.149466714     NA    12.74320  7 0.8881106
51     1 -0.066429049     NA    12.74320  7 0.8931704
52     1 -0.270339724     NA    12.74319  7 0.8726438
53     1 -0.423253072     NA    12.74317  7 0.8303169
54     1 -0.488411599     NA    12.74315  7 0.7745599
55     1 -0.438254545     NA    12.74316  7 0.7177103
56     1 -0.269189585     NA    12.74319  7 0.6741607
57     1 -0.832887547     NA    13.50735  8 0.5439182
58     1 -0.031445473     NA    13.50766  8 0.4895063
59     1  0.786301204     NA    13.50738  8 0.5370583
60     1  1.248530742     NA    13.50715  8 0.6635144
61     1  1.313100570     NA    13.50722  8 0.8207279
62     1  1.095042772     NA    13.50735  8 0.9674592
63     1  0.705771427     NA    13.50745  8 1.0767806
64     1  0.227275770     NA    13.50750  8 1.1333253
65     1 -1.137324646     NA    13.71745  9 1.0227181
66     1 -1.428021685     NA    13.71729  9 0.8666628
67     1 -1.440802010     NA    13.71716  9 0.6909683
68     1 -1.024199570     NA    13.71734  9 0.5380218
69     1 -0.142138020     NA    13.71781  9 0.4643515
70     1  0.819899192     NA    13.71748  9 0.5073097
71     1  1.374750280     NA    13.71716  9 0.6441497
72     1  1.465506700     NA    13.71725  9 0.8185913
73     1 -0.486903258     NA    12.74269 10 0.7615206
74     1 -0.409530873     NA    12.74270 10 0.7064838
75     1 -0.217862781     NA    12.74273 10 0.6678357
76     1  0.040946136     NA    12.74274 10 0.6569149
77     1  0.286770856     NA    12.74272 10 0.6771362
78     1  0.446325758     NA    12.74269 10 0.7222522
79     1  0.486151121     NA    12.74269 10 0.7794419
80     1  0.412522959     NA    12.74270 10 0.8343924
81     1  0.993691190     NA    13.40692 11 0.9694861
82     1  0.608809597     NA    13.40701 11 1.0667773
83     1  0.145614629     NA    13.40704 11 1.1125101
84     1 -0.334805910     NA    13.40704 11 1.1010451
85     1 -0.774791829     NA    13.40698 11 1.0337521
86     1 -1.108680786     NA    13.40688 11 0.9192824
87     1 -1.251252896     NA    13.40676 11 0.7752308
88     1 -1.094476332     NA    13.40676 11 0.6308846
89     1  0.623175625     NA    12.87249 12 0.6915717
90     1  0.705277732     NA    12.87248 12 0.7731433
91     1  0.619816882     NA    12.87251 12 0.8541432
92     1  0.412259897     NA    12.87255 12 0.9170446
93     1  0.133857888     NA    12.87257 12 0.9502752
94     1 -0.166010483     NA    12.87257 12 0.9483197
95     1 -0.439149242     NA    12.87255 12 0.9114922
96     1 -0.635930237     NA    12.87251 12 0.8459535
97     1  0.225407286     NA    12.75063 13 0.8841458
98     1  0.008569284     NA    12.75064 13 0.8984028
99     1 -0.209797711     NA    12.75063 13 0.8861417
100    1 -0.389729551     NA    12.75061 13 0.8495789
101    1 -0.492280801     NA    12.75059 13 0.7956868
102    1 -0.484251721     NA    12.75059 13 0.7358471
103    1 -0.350603470     NA    12.75062 13 0.6845079
104    1 -0.113446310     NA    12.75064 13 0.6558821
105    1  0.348268301     NA    12.75031 14 0.6841938
106    1  0.482952143     NA    12.75028 14 0.7353110
107    1  0.492189008     NA    12.75029 14 0.7950674
108    1  0.390706177     NA    12.75030 14 0.8490149
109    1  0.211551396     NA    12.75032 14 0.8857449
110    1 -0.006392916     NA    12.75033 14 0.8982456
111    1 -0.223196180     NA    12.75032 14 0.8842560
112    1 -0.398942477     NA    12.75030 14 0.8463104
113    1 -0.634636475     NA    12.82518 15 0.7735269
114    1 -0.562948733     NA    12.82519 15 0.7000127
115    1 -0.331209909     NA    12.82524 15 0.6448641
116    1  0.009207159     NA    12.82527 15 0.6249351
117    1  0.346437982     NA    12.82524 15 0.6469440
118    1  0.570618745     NA    12.82519 15 0.7034916
119    1  0.633995484     NA    12.82518 15 0.7774200
120    1  0.548432940     NA    12.82521 15 0.8496990

dummy_simple <- targets::tar_read(dummy_simple) |>
  rename(mother_id = sp)
