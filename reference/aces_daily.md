# Multilevel Daily Data Example

A data frame drawn from a daily diary study, conducted at Monash
University in 2017 where young adults old completed measures up to three
times per day (morning, afternoon, and evening) for about 12 days. Thus
each participant contributed about 36 observations to the dataset. To
protect participant confidentiality and anonymity, the data used here
were simulated from the original data, but in such a way as to preserve
the relations among variables and most features of the raw data.

## Usage

``` r
aces_daily
```

## Format

A data frame containing 19 variables.

- UserID:

  A unique identifier for each individual

- SurveyDay:

  The date each observation occured on

- SurveyInteger:

  The survey coded as an integer (1 = morning, 2 = afternoon, 3 =
  evening)

- SurveyStartTimec11:

  Survey start time, centered at time since 11am

- Female:

  A 0 or 1 variable, where 1 = female and 0 = male

- Age:

  Participant age in years, top coded at 25

- BornAUS:

  A 0 or 1 variable where 1 = born in Australia and 0 = born outside of
  Australia

- SES_1:

  Participants subjective SES, bottom coded at 4 and top coded at 8

- EDU:

  Participants level of education (1 = university graduate or higher, 0
  = less than university graduate

- SOLs:

  Self-reported sleep onset latency in minutes, morning survey only

- WASONs:

  Self-reported number of wakenings after sleep onset, top coded at 4,
  morning survey only

- STRESS:

  Overall stress ratings on a 0–10 scale, repeated 3x daily

- SUPPORT:

  Overall social support ratings on a 0–10 scale, repeated 3x daily

- PosAff:

  Positive affect ratings on a 1–5 scale, repeated 3x daily

- NegAff:

  Negative affect ratings on a 1–5 scale, repeated 3x daily

- COPEPrb:

  Problem focused coping on a 1–4 scale, repeated 1x daily at the
  evening survey

- COPEPrc:

  Emotional processing coping on a 1–4 scale, repeated 1x daily at the
  evening survey

- COPEExp:

  Emotional expression coping on a 1–4 scale, repeated 1x daily at the
  evening survey

- COPEDis:

  Mental disengagement coping on a 1–4 scale, repeated 1x daily at the
  evening survey
