 PARAM - a Pixel based Agro-hydrological model in R for water Accounting and Management. 

 It uses EPIC crop model, and the pixel based hydrological model is operated with multiple soil layers.
 Irrigation is triggered based on soil moisture deficit. 
 Blue-green water distinction is performed using Chukalla et al., 2015.

 Dynamic crop maps, corrected to reported crop area for each year is used for modeling. 
 In order to simulate the crop yield trends, dynamic crop parameters are used instead of a single parameter. 

 The model is set to use parallel computing in both windows and linux(HPC) environment. 

 main.R - to use in linux (HPC) using MPI.
 main_win.R - to use for windows environment. 

 More update coming soon ......

 References : Chukalla, A. D., Krol, M. S. & Hoekstra, A. Y. Green and blue water footprint reduction in irrigated agriculture: effect of irrigation techniques, irrigation strategies and mulching. Hydrol. Earth Syst. Sci. 19, 4877–4891 (2015).
