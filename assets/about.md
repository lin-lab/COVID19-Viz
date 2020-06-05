
## Website Usage

**Rt map tab**: This tab shows a map of the Rt by date for various
resolutions. Change the date by adjusting the slider. Change the
resolution (Country, US states, US counties, Canadian/Chinese/Australian
provinces) using the resolution dropdown menu, and click on a location
to see a line graph of the Rt over time. You can scroll to change the
zoom of the map and click-drag to move the map around. Below the map,
there is a table of Rts for the chosen date and resolution. This table
is by default sorted in descending order of Rt, but the sorting can be
changed by clicking a column header. Locations where Rt could not be
calculated are colored gray in the map and not shown in the table (see
Rt Method Description for more info).

Technical note: the blue line is a fitted loess curve to the Rt points,
and the error bars show the 95% posterior interval of Rt for that time
point.

**Compare Rt tab**: Select states/provinces, US counties, and countries
to compare their Rt over time. You can select a location by using the
dropdown menu. You can also type the name of the location. Multiple
locations for each category (states/provinces, counties, and countries)
can be chosen. After you click submit, the results will be displayed as
a line plot. Double-clicking a location on the right side of the plot
will show only its Rt curve---this can be undone by double-clicking
again. Single-clicking will remove a location’s Rt curve from the
display---this can be undone by single-clicking again. Some areas may
not appear in the plot because of insufficient data (see Rt Method
Description for more info).

**Explore States tab**: This tab shows an overview for all the counties
in a selected state. The plot on the left shows the change in Rt over
time for counties where Rt was able to be calculated. The map on the
right shows the Rt for the counties in the state for a selected date.
The date can be adjusted using the slider, and an animation can be shown
by clicking the blue “play” button. You can zoom in and out by scrolling
and you can move the map by clicking and dragging. Below the map, there
is a table of Rt for the date selected using the slider. This table is
by default sorted in descending order of Rt, but the sorting can be
changed by clicking a column header.

## Rt Method Description

We calculate and report the daily effective reproduction number
(Rt) to characterize the virus spread, defined as the expected number of
secondary infectious cases produced by a primary infectious case. Rt is
used to determine the potential for epidemic spread at a specific time t
under the control measures in place (Figure 1, Inglesby, T.V., 2020,
reproduced below). If Rt > 1, the virus will spread out and the disease
will become an epidemic; if Rt = 1, the virus will spread locally and
the disease is endemic; if Rt < 1, the virus will stop spreading and the
disease will disappear eventually.

<img src="Rt_explanation.png" alt="Figure 1: Explanation of Rt", width="800" />

To obtain the Rt estimate, different methods could be considered (Cori,
A., et al., 2013; Thompson, R.N., et al., 2019; Walllinga, J. and
Teunis, P., 2004; Zhang, J., et al., 2020). Here we focused on using the
EpiEstim method (Cori, A., et al., 2013; Thompson, R.N., et al., 2019)
to estimate the daily Rt value. The EpiEstim method requires the
following inputs (and outputs the daily Rt estimates):

+ Daily positive increase cases: we used data from Johns Hopkins
  University the Center for Systems Science and Engineering (JHU-CSSE)
  [Coronavirus Resource
  Center](https://github.com/CSSEGISandData/COVID-19) (Dong, E., et al,
  2020).

+ The time window of daily positive increase cases to be averaged
  (to smooth out the discreteness due to reporting). For our analysis, a
  7-day window was used by default.

+ The input parameter values of the distribution of the disease serial
  interval: We used a Gamma distribution with a mean of 5.2 days and a
  standard deviation of 5.1 days (He, X., et al, 2020).

We are also performing sensitivity analyses using different methods
(Walllinga, J. and Teunis, P., 2004; Zhang, J., et al., 2020) to
evaluate the robustness of Rt estimates obtained from EpiEstim and these
methods.

We obtained Rt estimates for each day since March 19, 2020 for each
country in the JHU dataset. We also state/province level daily Rt
estimates for the USA, Australia, Canada, and China, as well as
county-level daily Rt estimates for the US.

Rt estimation becomes unstable when there are only a few new cases per
day or when the total number of cases is small. Because of low
population size or lack of reporting, this is the case for many rural
counties in the US as well as many countries with underdeveloped
healthcare infrastructure. Therefore, we do not show the Rt value when
this is the case.


## References

1. Inglesby, T.V., 2020. Public health measures and the reproduction
   number of SARS-CoV-2. *JAMA*, 323(21), pp.2186-2187. doi:
   [10.1001/jama.2020.7878](https://doi.org/10.1001/jama.2020.7878)

2. Cori, A., Ferguson, N.M., Fraser, C. and Cauchemez, S., 2013. A new
   framework and software to estimate time-varying reproduction numbers
   during epidemics. *American Journal of Epidemiology*, 178(9),
   pp.1505-1512. doi: [https://doi.org/10.1093/aje/kwt133](https://doi.org/10.1093/aje/kwt133)

3. Thompson, R.N., Stockwin, J.E., van Gaalen, R.D., Polonsky, J.A.,
   Kamvar, Z.N., Demarsh, P.A., Dahlqwist, E., Li, S., Miguel, E.,
   Jombart, T. and Lessler, J., 2019. Improved inference of time-varying
   reproduction numbers during infectious disease outbreaks.
   *Epidemics*, 29, p.100356. doi:
   [10.1016/j.epidem.2019.100356](https://doi.org/10.1016/j.epidem.2019.100356)

4. Wallinga, J. and Teunis, P., 2004. Different epidemic curves for
   severe acute respiratory syndrome reveal similar impacts of control
   measures. *American Journal of Epidemiology*, 160(6), pp.509-516.
   doi: [10.1093/aje/kwh255](https://doi.org/10.1093/aje/kwh255)

5. Zhang, J., Litvinova, M., Wang, W., Wang, Y., Deng, X., Chen, X., Li,
   M., Zheng, W., Yi, L., Chen, X. and Wu, Q., 2020. Evolving
   epidemiology and transmission dynamics of coronavirus disease 2019
   outside Hubei province, China: a descriptive and modelling study.
   *The Lancet Infectious Diseases*. doi:
   [10.1016/S1473-3099(20)30230-9](https://doi.org/10.1016/S1473-3099%2820%2930230-9)

6. Dong, E., Du, H. and Gardner, L., 2020. An interactive web-based
   dashboard to track COVID-19 in real time. *The Lancet Infectious
   Diseases*, 20(5), pp.533-534. doi:
   [10.1016/S1473-3099(20)30120-1](https://doi.org/10.1016/S1473-3099%2820%2930120-1)

7. He, X., Lau, E.H., Wu, P., Deng, X., Wang, J., Hao, X., Lau, Y.C.,
   Wong, J.Y., Guan, Y., Tan, X. and Mo, X., 2020. Temporal dynamics in
   viral shedding and transmissibility of COVID-19. *Nature Medicine*,
   26(5), pp.672-675. doi:
   [10.1038/s41591-020-0869-5](https://doi.org/10.1038/s41591-020-0869-5)

