Hotel bookings
================
Maria Dermit
r.Sys.Date()

Lets build a model for hotel bookings. Which hotels have kids, per
state

## Explore the data

``` r
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels %>% count(is_canceled)
```

    ## # A tibble: 2 x 2
    ##   is_canceled     n
    ##         <dbl> <int>
    ## 1           0 75166
    ## 2           1 44224

``` r
#we are going to keep only the non-cancelled reservations. 
hotel_stays<-hotels  %>% filter(is_canceled==0) %>% 
  mutate(children=case_when(children+babies>0~ "children",
                             TRUE ~ "none"),
         required_car_parking_spaces=case_when(required_car_parking_spaces>0~"parking",TRUE ~"none")) %>% select(-is_canceled,-reservation_status,-babies)
                      
hotel_stays%>% count(children)
```

    ## # A tibble: 2 x 2
    ##   children     n
    ##   <chr>    <int>
    ## 1 children  6073
    ## 2 none     69093

``` r
library(skimr)
#plots a summary of the tibble!!
skim(hotel_stays)
```

|                                                  |              |
| :----------------------------------------------- | :----------- |
| Name                                             | hotel\_stays |
| Number of rows                                   | 75166        |
| Number of columns                                | 29           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |              |
| Column type frequency:                           |              |
| character                                        | 14           |
| Date                                             | 1            |
| numeric                                          | 14           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |              |
| Group variables                                  | None         |

Data summary

**Variable type:
character**

| skim\_variable                 | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :----------------------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| hotel                          |          0 |              1 |  10 |  12 |     0 |         2 |          0 |
| arrival\_date\_month           |          0 |              1 |   3 |   9 |     0 |        12 |          0 |
| children                       |          0 |              1 |   4 |   8 |     0 |         2 |          0 |
| meal                           |          0 |              1 |   2 |   9 |     0 |         5 |          0 |
| country                        |          0 |              1 |   2 |   4 |     0 |       166 |          0 |
| market\_segment                |          0 |              1 |   6 |  13 |     0 |         7 |          0 |
| distribution\_channel          |          0 |              1 |   3 |   9 |     0 |         5 |          0 |
| reserved\_room\_type           |          0 |              1 |   1 |   1 |     0 |         9 |          0 |
| assigned\_room\_type           |          0 |              1 |   1 |   1 |     0 |        10 |          0 |
| deposit\_type                  |          0 |              1 |  10 |  10 |     0 |         3 |          0 |
| agent                          |          0 |              1 |   1 |   4 |     0 |       315 |          0 |
| company                        |          0 |              1 |   1 |   4 |     0 |       332 |          0 |
| customer\_type                 |          0 |              1 |   5 |  15 |     0 |         4 |          0 |
| required\_car\_parking\_spaces |          0 |              1 |   4 |   7 |     0 |         2 |          0 |

**Variable type:
Date**

| skim\_variable            | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------------------ | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| reservation\_status\_date |          0 |              1 | 2015-07-01 | 2017-09-14 | 2016-09-01 |       805 |

**Variable type:
numeric**

| skim\_variable                    | n\_missing | complete\_rate |    mean |    sd |      p0 |    p25 |    p50 |  p75 | p100 | hist  |
| :-------------------------------- | ---------: | -------------: | ------: | ----: | ------: | -----: | -----: | ---: | ---: | :---- |
| lead\_time                        |          0 |              1 |   79.98 | 91.11 |    0.00 |    9.0 |   45.0 |  124 |  737 | ▇▂▁▁▁ |
| arrival\_date\_year               |          0 |              1 | 2016.15 |  0.70 | 2015.00 | 2016.0 | 2016.0 | 2017 | 2017 | ▃▁▇▁▆ |
| arrival\_date\_week\_number       |          0 |              1 |   27.08 | 13.90 |    1.00 |   16.0 |   28.0 |   38 |   53 | ▆▇▇▇▆ |
| arrival\_date\_day\_of\_month     |          0 |              1 |   15.84 |  8.78 |    1.00 |    8.0 |   16.0 |   23 |   31 | ▇▇▇▇▆ |
| stays\_in\_weekend\_nights        |          0 |              1 |    0.93 |  0.99 |    0.00 |    0.0 |    1.0 |    2 |   19 | ▇▁▁▁▁ |
| stays\_in\_week\_nights           |          0 |              1 |    2.46 |  1.92 |    0.00 |    1.0 |    2.0 |    3 |   50 | ▇▁▁▁▁ |
| adults                            |          0 |              1 |    1.83 |  0.51 |    0.00 |    2.0 |    2.0 |    2 |    4 | ▁▂▇▁▁ |
| is\_repeated\_guest               |          0 |              1 |    0.04 |  0.20 |    0.00 |    0.0 |    0.0 |    0 |    1 | ▇▁▁▁▁ |
| previous\_cancellations           |          0 |              1 |    0.02 |  0.27 |    0.00 |    0.0 |    0.0 |    0 |   13 | ▇▁▁▁▁ |
| previous\_bookings\_not\_canceled |          0 |              1 |    0.20 |  1.81 |    0.00 |    0.0 |    0.0 |    0 |   72 | ▇▁▁▁▁ |
| booking\_changes                  |          0 |              1 |    0.29 |  0.74 |    0.00 |    0.0 |    0.0 |    0 |   21 | ▇▁▁▁▁ |
| days\_in\_waiting\_list           |          0 |              1 |    1.59 | 14.78 |    0.00 |    0.0 |    0.0 |    0 |  379 | ▇▁▁▁▁ |
| adr                               |          0 |              1 |   99.99 | 49.21 |  \-6.38 |   67.5 |   92.5 |  125 |  510 | ▇▆▁▁▁ |
| total\_of\_special\_requests      |          0 |              1 |    0.71 |  0.83 |    0.00 |    0.0 |    1.0 |    1 |    5 | ▇▁▁▁▁ |

``` r
hotel_stays %>% 
  mutate(arrival_date_month=factor(arrival_date_month,
                                   levels =month.name )) %>% 
  count(hotel, arrival_date_month, children) %>% 
  group_by(hotel, children) %>% 
  mutate(proportion=n/sum(n)) %>% 
  ggplot(aes(arrival_date_month,proportion,fill=children))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels=scales::percent_format())+
  facet_wrap(~hotel, nrow = 2)
```

![](Hotelbookings_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#across the months, ppl with children are more abundant in summer. And this trend is even more pronounce for resort hotels. 
```

``` r
#do pppl that arrive with children need a car?
hotel_stays %>% 

  count(hotel, required_car_parking_spaces, children) %>% 
  group_by(hotel, children) %>% 
  mutate(proportion=n/sum(n)) %>% 
  ggplot(aes(required_car_parking_spaces,proportion,fill=children))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels=scales::percent_format())+
  facet_wrap(~hotel, nrow = 2)
```

![](Hotelbookings_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
library(GGally)
#to do pair plots
#we select children, bc that is the thing that we are going to be predicting
hotel_stays %>% 
  select(children,adr,required_car_parking_spaces,total_of_special_requests) %>% 
  ggpairs(mapping = aes(color=children))
```

![](Hotelbookings_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#ggpairs looks at all the data and decide what plot to make. 
#it does a multipanel plot for us 
```

\#build models with recipes

``` r
#first create a modeling dataframe
hotels_df<-hotel_stays %>% 
  select(children,hotel,arrival_date_day_of_month,meal,adr,adults,required_car_parking_spaces,
         total_of_special_requests,stays_in_week_nights,  stays_in_weekend_nights) %>% 
  mutate_if(is.character,factor)
```

``` r
library(tidymodels)
set.seed(1234)
hotel_split<-initial_split(hotels_df)

hotel_train<-training(hotel_split)
hotel_testing<-testing(hotel_split)

#predict children with everything else. Children is the outcome in the recipe
hotel_rec<-recipe(children ~.,data=hotel_train ) %>% 
  #need to balance the recipe, ML loves to say"look at those ppl without children XD. We are trying to make children variable even. then we make dummy variables, which turns all factors into numbers.bc some of the numbers are very large and others are very small, we need to normalise those. last step ois to prep the recipe, which goes to the training data and does the training. 
  step_downsample(children) %>% 
  step_dummy(all_nominal(),-all_outcomes()) %>% 
  step_zv(all_numeric()) %>% 
  step_normalize(all_numeric()) %>% 
  prep()

#this is to squeeze data out of the recipe
juice(hotel_rec)
```

    ## # A tibble: 9,176 x 13
    ##    arrival_date_da…    adr adults total_of_specia… stays_in_week_n…
    ##               <dbl>  <dbl>  <dbl>            <dbl>            <dbl>
    ##  1            -1.72  0.523  0.232            0.107            0.792
    ##  2            -1.49  0.495  0.232           -0.974           -0.290
    ##  3            -1.49 -0.369  0.232            1.19             1.33 
    ##  4            -1.38 -0.414 -1.82             1.19             0.792
    ##  5            -1.38 -0.779  0.232            1.19             1.33 
    ##  6            -1.38  0.269  0.232            1.19             1.33 
    ##  7            -1.38  0.430  0.232            0.107            1.87 
    ##  8            -1.27  0.675  0.232            0.107            1.33 
    ##  9            -1.27  0.848  0.232            0.107            1.33 
    ## 10            -1.15  0.720  0.232           -0.974            0.792
    ## # … with 9,166 more rows, and 8 more variables: stays_in_weekend_nights <dbl>,
    ## #   children <fct>, hotel_Resort.Hotel <dbl>, meal_FB <dbl>, meal_HB <dbl>,
    ## #   meal_SC <dbl>, meal_Undefined <dbl>,
    ## #   required_car_parking_spaces_parking <dbl>

``` r
juice(hotel_rec) %>%  count(children)
```

    ## # A tibble: 2 x 2
    ##   children     n
    ##   <fct>    <int>
    ## 1 children  4588
    ## 2 none      4588

``` r
#we hve successfully downsample the data!
#####all this is data preprocessing and feature engineering. This prevents leaking of data from training data into testing data

#now we can take this recipe and apply it to new data with bake. This gives me actual data. 
test_proc<-bake(hotel_rec, new_data=hotel_testing)
```

\#lets train some model with parnsip

``` r
###create a model, chose an engine an fit. Parsnip

#we are gong to use kknn model is a classigication model cuz the hotels either do not have children or have children. kknn is sensitive to centering and scaling ( the big and small numbers issue)
knn_spec<-nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

#now we can fit this model
 knn_fit<- knn_spec %>% 
    fit(children ~ ., 
        data=juice(hotel_rec))
knn_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  4.2s 
    ## 
    ## Call:
    ## kknn::train.kknn(formula = formula, data = data, ks = 5)
    ## 
    ## Type of response variable: nominal
    ## Minimal misclassification: 0.2724499
    ## Best kernel: optimal
    ## Best k: 5

``` r
tree_spec<-decision_tree() %>% 
  set_engine("rpart") %>%
    set_mode("classification")
tree_fit<-tree_spec %>% 
  fit(children ~.,
      data=juice(hotel_rec))
```

\#evaluate models

``` r
set.seed(1234)
validation_splits<-mc_cv(juice(hotel_rec),prep=0.9,strata=children)

knn_res<-fit_resamples(knn_spec, children ~ ., resamples = validation_splits, 
    control = control_resamples(save_pred = TRUE))
  #is fitting the model kknn model with the validation_splits data. This function is not doing any tunning. We are only stimating how good our model is fitting our data. fit_resamples does not fit but EVALUATES!!!
knn_res %>%  collect_metrics()
```

    ## # A tibble: 2 x 5
    ##   .metric  .estimator  mean     n std_err
    ##   <chr>    <chr>      <dbl> <int>   <dbl>
    ## 1 accuracy binary     0.724    25 0.00131
    ## 2 roc_auc  binary     0.788    25 0.00129

``` r
##
tree_res<-fit_resamples(tree_spec, children ~ ., resamples = validation_splits, 
    control = control_resamples(save_pred = TRUE))
  #is fitting the model kknn model with the validation_splits data. This function is not doing any tunning. We are only stimating how good our model is fitting our data. fit_resamples does not fit but EVALUATES!!!
tree_res %>%  collect_metrics()
```

    ## # A tibble: 2 x 5
    ##   .metric  .estimator  mean     n std_err
    ##   <chr>    <chr>      <dbl> <int>   <dbl>
    ## 1 accuracy binary     0.721    25 0.00167
    ## 2 roc_auc  binary     0.739    25 0.00176

``` r
#bc knn_res ROC_AUC mean is bigger that that one of tree_res, knn is doing better
```

``` r
knn_res %>% 
  unnest(.predictions) %>% 
  mutate(model="kknn") %>% 
  bind_rows(
    tree_res %>% 
  unnest(.predictions) %>% 
  mutate(model="rpart")) %>% 
  group_by(model) %>% 
  roc_curve(children,.pred_children) %>% 
  autoplot()
```

![](Hotelbookings_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#kknn performs better. Lets do a confusion matrix. The areas show which ones are correctly and incorrectly predicted. For instance, top left box, shows hotel that had childrens that were correctly predicted to have children.
knn_res %>% 
  unnest(.predictions) %>% 
  conf_mat(children,.pred_class) %>% 
  autoplot()
```

![](Hotelbookings_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
#going back to the test data to see how this test data would perform on new data. Predict from parsnip always returns a tibble

 knn_fit %>% 
  predict(new_data=test_proc,type="prob") %>% 
#this is the probability that in everyexample in the test set this is the probability hotel will have children and not 
  mutate(truth=hotel_testing$children) %>% 
#this is the true answer
  roc_auc(truth, .pred_children)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.781

``` r
#this roc_auc is very similar to the one in the validation set, so it is an excellent job!!

#we have used recipes for preprocessing, then validation spli ts for evaluating and at the very end test data to see how we expect our model to do on new data. 
```
