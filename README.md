# Reproducing doomsday models in R

The repository hist_gc_sysdyn (short for history_globalcatastrophes_systemdynamics) contains R codes of published system-dynamics models of global catrastrophes, doomsday scenarios, and other existential risks. 

**Rworld2**: Translation of the World2 model from DYNAMO to R, based on the source codes given in Forrester (1971), chapter 3 and appendix B. The World2 model can be considered the first computer-based doomsday model. It is a 5th-order differential equation model with population, natural resource, capital investment, capital-investment-in-agriculture fraction, and pollution as main variables. The R code has been benchmarked against Fig. 4-1 of Forrester (1971). Read more in Mignan (2020).

![Benchmarking R versus DYNAMO](https://github.com/amignan/hist_gc_sysdyn/blob/master/fig_benchmarking_RvsDYNAMO.jpg)

**Rword3**: Extension of world2, discussed in 'The Limits to Growth' (Meadows et al., 1972; Meadows, 1974). Not yet available.

## References
Forrester JW (1971), World Dynamics. Wright-Allen Press, Inc., Cambridge, 144 pp.

Meadows DH, Meadows DL, Randers J, Behrens III WW (1972), The Limits to Growth. Universe Books, New York, 205 pp.

Meadows DL (1974), Dynamics of Growth in a Finite World. Wright-Allen Press, Cambridge, 637 pp.

Mignan A (2020), [World2 model, from DYNAMO to R](https://towardsdatascience.com/world2-model-from-dynamo-to-r-2e44fdbd0975). Towards Data Science, Medium