extensions [table]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Variable Definition                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [Y_d_sum Y_s_IS
         tick-list 
         Y_s_0 Y_s_1 Y_s_2 
         CSX_0 CSX_1 CSX_2  
         X_0 X_1 X_2
         W_0 W_1 W_2
         W_0_t0 W_1_t0 W_2_t0
         h_l c c_k c_k_t
         phi_IS_list_
         xx yy z e_1_
         ref_IS_M1 ref_IS_M2 ref_OP
         ran0_h ran1_h ran2_h
         ran0_x_s ran1_x_s ran2_x_s
         m0 m1 m2
         ex0 ex1 ex2
         s0 s1 s2 
         t0 t1 t2 
         ]
patches-own [y h x x_p x_t x_s x_d x_IS c_PC c_GC d_ij p_ref 
             phi_IS phi_IS_MS1 phi_IS_MS2 phi_IS_MS3 phi_IS_MS3.1 phi_IS_SU phi_IS_EX 
             name x_d_0 x_d_1 x_d_2 prod_IS links_
             d_iv_IS x_T_IS cap_IS cap_0 
             city_ID d_all x_dd]
breed [cities city]
breed [producers producer]
cities-own [y_d_j]
producers-own []

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Model Initialisation                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  setup-cities
  setup-values
  setup-patches
  ;setup-producers
  reset-ticks
end

to setup-turtles
  delete-producers
  calculate-new-patch-variables
  ;setup-various 
  ;setup-producers
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Defining the Go procedure                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go 
   
   calculate-new-welfare-per-tick
   
;; Strategy application
   if SU = true
   [ support-module ]
   
   calculate-new-profit-per-tick
   
   if EX = true
   [ exit-module ]
  

;; Defining the coalition form     
   if PA = 0
   [ set-producer_IS ]
   if PA = 1 
   [ set tick-list [0 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3]      
      if item ticks tick-list = 1 [set-producer-city0]
      if item ticks tick-list = 2 [set-producer-city1]
      if item ticks tick-list = 3 [set-producer-city2] 
    ]
   if PA = 2 
   [ set tick-list [0 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2]      
      if item ticks tick-list = 1 [set-producer-city01]
      if item ticks tick-list = 2 [set-producer-city2] 
    ]
   if PA = 3 
   [ set tick-list [0 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2]      
      if item ticks tick-list = 1 [set-producer-city02]
      if item ticks tick-list = 2 [set-producer-city1]
    ]
   if PA = 4 
   [ set tick-list [0 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 2]      
      if item ticks tick-list = 1 [set-producer-city12]
      if item ticks tick-list = 2 [set-producer-city0]
   ]
  
   calculate-output-variables

   tick

;; Defining the stop criteria depending on the coalition form
   if PA = 0 and Y_s_IS >= Y_d_sum
   [stop] 
   if PA = 1 and Y_s_0 >= Y_D_0 and Y_s_1 >= Y_D_1 and Y_s_2 >= Y_D_2
   [stop] 
   if PA = 2 and (Y_s_0 + Y_s_1) >= (Y_D_0 + Y_D_1) and Y_s_2 >= Y_D_2
   [stop] 
   if PA = 3 and (Y_s_0 + Y_s_2) >= (Y_D_0 + Y_D_2) and Y_s_1 >= Y_D_1
   [stop] 
   if PA = 4 and (Y_s_1 + Y_s_2) >= (Y_D_1 + Y_D_2) and Y_s_0 >= Y_D_0
   [stop] 

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Procedures for the setup                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-cities
  create-cities 3
  ask cities 
    [
    set color black
    set shape "circle"
    set size 2
    ]
  ask city 0
  [setxy 0 4
   set y_d_j Y_D_0
   create-link-with city 1]
  ask city 1
  [setxy 8 -4
   set y_d_j Y_D_1
   create-link-with city 2]
  ask city 2
  [setxy -8 -4
   set y_d_j Y_D_2
   create-link-with city 0] 
  set Y_d_sum sum [y_d_j] of cities 
end

to setup-values
  set t0 0
  set t1 0
  set t2 0
  set s0 0
  set s1 0
  set s2 0
  set ex0 0
  set ex1 0
  set ex2 0
  set ran0_h random-float 1
  set ran1_h random-float 1 
  set ran2_h random-float 1  
  set ran0_x_s random-float 1
  set ran1_x_s random-float 1 
  set ran2_x_s random-float 1   
end

to setup-patches
  ask patches
  [
    ;; Distance calculation
    let d_0 distance city 0
    let d_1 distance city 1
    let d_2 distance city 2
     
    ;; Defining the catchment area 
    ifelse d_0 < d_1 and d_0 <= d_2 and d_2 > 6
    [set city_ID 0]
    [ifelse d_1 < d_2
      [set city_ID 1]
      [set city_ID 2]]
       ; Correction so that all all cities (consumer centres) get the same number of cells
       if pxcor = -1 and pycor = -3 [set city_ID 2]
       if pxcor = -6 and pycor = 2 [set city_ID 2]
       if pxcor = -2 and pycor = -2 [set city_ID 2]
       if pxcor = -7 and pycor = 3 [set city_ID 2]
       if pxcor = -8 and pycor = 4 [set city_ID 2]
       if pxcor = -9 and pycor = 5 [set city_ID 2]
       if pxcor = 1 and pycor = -2 [set city_ID 1]
      
   ;, Defining distance layer for the land-use restriction policy
   if city_ID = 0 [set d_all 25 - d_0]
   if city_ID = 1 [set d_all 25 - d_1]
   if city_ID = 2 [set d_all 25 - d_2]

   ;; Calculation of the distance-depenent external costs
   set x_d_0 alfa * (d_0 + 1) ^ (- (beta)) * Y_D_0 / (y_d_sum / 3) 
   set x_d_1 alfa * (d_1 + 1) ^ (- (beta)) * Y_D_1 / (y_d_sum / 3)
   set x_d_2 alfa * (d_2 + 1) ^ (- (beta)) * Y_D_2 / (y_d_sum / 3)

   ;; Defining the yield harvest potential layer
   
   set h (random h_max + (0.25 * h_max))  
  
      ; Implementation of random regional harvest variations
      if ran0_h > 0.66
      [ if city_ID = 0 [set h (random (h_max - (0.25 * h_max)) + (0.5 * h_max))] ]
      if ran0_h < 0.33
      [ if city_ID = 0  [set h (random (h_max - (0.5 * h_max)) + (0.5 * h_max))] ]

      if ran1_h > 0.66
      [ if city_ID = 1 [set h (random (h_max - (0.25 * h_max)) + (0.5 * h_max))] ]
      if ran1_h < 0.33
      [ if city_ID = 1  [set h (random (h_max - (0.5 * h_max)) + (0.5 * h_max))] ]

      if ran2_h > 0.66
      [ if city_ID = 2 [set h (random (h_max - (0.25 * h_max)) + (0.5 * h_max))] ]
      if ran2_h < 0.33
      [ if city_ID = 2 [set h (random (h_max - (0.5 * h_max)) + (0.5 * h_max))] ]
  
 
   ;; Defining the site dependent external cost layer
   set x_s (random x_s_max )
   
      ; Implementation of random regional external cost variations
      if ran0_x_s > 0.66
      [ if city_ID = 0 [set x_s (random (x_s_max - (0.25 * x_s_max)) + (0.5 * x_s_max))] ]
       if ran0_x_s < 0.33
      [ if city_ID = 0  [set x_s (random (x_s_max - (0.5 * x_s_max)) + (0.5 * x_s_max))] ]

      if ran1_x_s > 0.66
      [ if city_ID = 1 [set x_s (random (x_s_max - (0.25 * x_s_max)) + (0.5 * x_s_max))] ]
      if ran1_x_s < 0.33
      [ if city_ID = 1  [set x_s (random (x_s_max - (0.5 * x_s_max)) + (0.5 * x_s_max))] ]

      if ran2_x_s > 0.66
      [ if city_ID = 2 [set x_s (random (x_s_max - (0.25 * x_s_max)) + (0.5 * x_s_max))] ]
       if ran2_x_s < 0.33
      [ if city_ID = 2 [set x_s (random (x_s_max - (0.5 * x_s_max)) + (0.5 * x_s_max))] ] 

   
   set x_s x_s - min [x_s] of patches  
  
  ]
   
   ;; Definition of the value diffusion of the landscape raster variables 
    repeat 1
  [ ask patches
    [ set x_s [x_s] of one-of neighbors4 
     
     ]]
    repeat 2
    [ask patches
    [set h [h] of one-of neighbors4 
    ]]
  diffuse h 0.75
  

  ;; Restricting the cells at the landscape fringe
  ask patches
  [ 
  if pxcor = 20 or pxcor = -20 
  [set h 0]
  if pycor = 14 or pycor = -14 
  [set h 0]
  set name (patch pxcor pycor)  
  
  ;; Calculation of the electricty potential per cell depending on the performance parameter
  set h_l l
  set y h * h_l
  ;; Calculation of the final distance-dependent external costs
  let list_x_d_all (list x_d_0 x_d_1 x_d_2)
  set x_dd (sum list_x_d_all) - min [x_d_0] of patches - min [x_d_1] of patches - min [x_d_2] of patches   
  ]
  
  ask patches
[
  ;; Correction of the distance-dependent external costs to rule out negative costs
  set x_d x_dd - min[x_dd] of patches
  ;; Defining the production cost 
  set c_PC h_l 
  ;; Calculation of the spatial external costs per generation infrastructure
  set x_p (x_s + x_d)  
  ;; Setting the landscape color
  set pcolor scale-color green x_s (max [x_s] of patches) 0
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Procedures for the setup-patches                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to delete-producers
ask turtles with[breed = producers][die]
set ref_IS_M1 0
set ref_IS_M2 0
set ref_OP 0
set ex0 0
set ex1 0
set ex2 0
end

to setup-various
ask patches
[set pcolor scale-color blue phi_IS (max [phi_IS] of patches) (0)
  ]
end
  
to calculate-new-patch-variables  
   
ask patches
[
    ;; Count producer per cell
    set prod_IS count producers-here with [color = 15]
    ;; Setting the grid capacity to the inital state 
    set cap_IS cap_0
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Procedures for the Go Preparation                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calculate-new-profit-per-tick  
   
;; Calculation of the profit per cell

ask patches
[
    ;; Calculation of the overall external costs
    set x_IS x_p + x_T_IS * theta    
    ;; Calculation of the profits per cell per scenario Market I
    set phi_IS_MS1 p * y - c_PC * w_PC - d_iv_IS * w_GC
    ;; Calculation of the profits per cell per scenario Market II
    set phi_IS_MS2 p * y - c_PC * w_PC - d_iv_IS * w_GC - w_GE * cap_IS * grid_ratio
    ;; Calculation of the profits per cell per scenario Reference Yield Model
    set omega 1 - ((mean [h] of patches) * psi)
    set p_ref psi * h + omega
    set phi_IS_MS3 p_ref * y - c_PC * w_PC - d_iv_IS * w_GC
    ;; Calculation of the profits per cell per scenario Reference Yield Model with boundaries
    ifelse p_ref > 1.2
        [set p_ref 1.2]
        [set p_ref p_ref]
    ifelse p_ref < 0.5
        [set p_ref 0.5]
        [set p_ref p_ref]
    set phi_IS_MS3.1 p_ref * y - c_PC * w_PC - d_iv_IS * w_GC
    ;; Calculation of the profits per cell per scenario Reference Yield Model with boundaries
    if city_id = 0
         [set phi_IS_SU (p_ref + s0) * y - c_PC * w_PC - d_iv_IS * w_GC]
    if city_id = 1
         [set phi_IS_SU (p_ref + s1) * y - c_PC * w_PC - d_iv_IS * w_GC]
    if city_id = 2
         [set phi_IS_SU (p_ref + s2) * y - c_PC * w_PC - d_iv_IS * w_GC]
         
    ;; Calculation of the profits per cell per scenario Reference Yield Model with boundaries
    if city_id = 0
         [set phi_IS_EX (p_ref - t0) * y - c_PC * w_PC - d_iv_IS * w_GC]
    if city_id = 1
         [set phi_IS_EX (p_ref - t1) * y - c_PC * w_PC - d_iv_IS * w_GC]
    if city_id = 2
         [set phi_IS_EX (p_ref - t2) * y - c_PC * w_PC - d_iv_IS * w_GC]
    
    ;; Defining the applied profit depending on the scenario setting
    set phi_IS phi_IS_MS1
    if MS2 = true   [set phi_IS phi_IS_MS2]
    if MS3 = true   [set phi_IS phi_IS_MS3]
    if MS3.1 = true [set phi_IS phi_IS_MS3.1]
    if SU = true    [set phi_IS phi_IS_SU]
    if EX = true    [set phi_IS phi_IS_EX]
    

]       

end

to calculate-new-welfare-per-tick

ask patches 
[    
    ;; Defining the regional multiplier parameters
    set m0 m * 0.5
    set m1 m
    set m2 m * 1.5
    
    ;; Counting the producer per scenario
    set prod_IS count producers-here with [color = 15]

    ;; Calculation of the spatial externalities per city (consumer centre)
    set X_0 sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 0)] * delta * (Y_D_0 / Y_D_sum)
    set X_1 sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 1)] * delta * (Y_D_1 / Y_D_sum)
    set X_2 sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 2)] * delta * (Y_D_2 / Y_D_sum)
    
    ;; Calculation of the regional utility per city (consumer centre)
    set CSX_0 (Y_D_0 * ((p_max - p - s0 )/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 0)]) * m0 - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 0)] * delta) - ref_IS_M1 * (Y_D_0 / Y_D_sum) * w_GE
    set CSX_1 (Y_D_1 * ((p_max - p - s1 )/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 1)]) * m1 - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 1)] * delta) - ref_IS_M1 * (Y_D_1 / Y_D_sum) * w_GE
    set CSX_2 (Y_D_2 * ((p_max - p - s2 )/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 2)]) * m2 - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 2)] * delta) - ref_IS_M1 * (Y_D_2 / Y_D_sum) * w_GE
    
    ;; Calculation of the overall welfare per city (consumer centre)
    set W_0 CSX_0 + sum[phi_IS]  of patches with [(prod_IS = 1) and (city_id = 0)] - ref_IS_M2 * (Y_D_0 / Y_D_sum) * w_GE
    set W_1 CSX_1 + sum[phi_IS]  of patches with [(prod_IS = 1) and (city_id = 1)] - ref_IS_M2 * (Y_D_1 / Y_D_sum) * w_GE 
    set W_2 CSX_2 + sum[phi_IS]  of patches with [(prod_IS = 1) and (city_id = 2)] - ref_IS_M2 * (Y_D_2 / Y_D_sum) * w_GE 
    
    if ticks = 1
    [set W_0_t0 W_0
     set W_1_t0 W_1
     set W_2_t0 W_2 
    ]
    
    ;; Mark restricted cells
    set pcolor scale-color blue phi_IS (max [phi_IS] of patches) (-1)
    if d_iv_IS > 800 [set pcolor 58]
]
end
  
to exit-module
  ask patches
  [
  if X_0  >= W_0_t0 * lambda 
  [if prod_IS = 0 and city_ID = 0 
    [;set phi_IS -999
     set t0 t
     set ex0 1]]
  if X_1  >= W_1_t0 * lambda 
  [if prod_IS = 0 and city_ID = 1 
    [;set phi_IS -999
     set t1 t
     set ex1 1]]
  if X_2  >= W_2_t0 * lambda 
  [if prod_IS = 0 and city_ID = 2 
    [;set phi_IS -999
     set t2 t
     set ex2 1]]
  ]
end 


to support-module
  ask patches
  [
  if W_0 + X_0 <= W_0_t0 * gamma
  [if prod_IS = 0 and city_ID = 0 [set s0 s]]
  if W_1 + X_1  <= W_1_t0 * gamma
  [if prod_IS = 0 and city_ID = 1 [set s1 s]]
  if W_2 + X_2 <= W_2_t0 * gamma
  [if prod_IS = 0 and city_ID = 2 [set s2 s]]
  ]
end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Procedures for setting Producers                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; Creating a producer depending on the coalition form

to set-producer_IS
   
   if Y_s_IS < Y_d_sum
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end 

to set-producer-city0
   
   ;; Erstellen eines Produzenten nach Marktszenario 
   if Y_s_0 <= Y_D_0
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0 and city_ID = 0]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end 

to set-producer-city1
   
   ;; Erstellen eines Produzenten nach Marktszenario 
   if Y_s_1 <= Y_D_1
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0 and city_ID = 1]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end 

to set-producer-city2
   
   ;; Erstellen eines Produzenten nach Marktszenario 
   if Y_s_2 <= Y_D_2
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0 and city_ID = 2]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end 

to set-producer-city01
   
   ;; Erstellen eines Produzenten nach Marktszenario 
   if (Y_s_0 + Y_s_1) <= (Y_D_0 +  Y_D_1)
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0 and city_ID < 2]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end

to set-producer-city02
   
   ;; Erstellen eines Produzenten nach Marktszenario 
   if (Y_s_0 + Y_s_2) <= (Y_D_0 +  Y_D_2)
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0 and city_ID != 1]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end

to set-producer-city12
   
   ;; Erstellen eines Produzenten nach Marktszenario 
   if (Y_s_1 + Y_s_2) <= (Y_D_1 +  Y_D_2)
   [
   create-producers 1
   [ 
    ;; Define basic properties of a producer
    set color 15
    set shape "instrument"
    set size 5
    ;; Producer occupies cell with highest profit
    set phi_IS_list_ reverse sort-by [?1 < ?2] [phi_IS] of patches with [prod_IS = 0 and city_ID > 0]
    set z item 0 phi_IS_list_
    set xx [name] of patches with [phi_IS = z and prod_IS = 0]
    set yy first xx   
    let x_cor_IS [pxcor] of patches with [name = yy]
    let y_cor_IS [pycor] of patches with [name = yy]
    setxy (item 0 x_cor_IS)  (item 0 y_cor_IS)
   ]
   ]
 
 end


to calculate-output-variables
   
   ask patches
   [ 
   
   ;; Count the producer per cell
   set prod_IS count producers-here with [color = 15]
   
   ;; Calculation of the electricy capacities
   set y_s_IS sum [y] of patches with [prod_IS = 1]
   set y_s_0 sum [y] of patches with [prod_IS = 1 and city_ID = 0]
   set y_s_1 sum [y] of patches with [prod_IS = 1 and city_ID = 1]
   set y_s_2 sum [y] of patches with [prod_IS = 1 and city_ID = 2]
   
   ;; Calculation of the regional utility per city (Consumer Centre)
   set CSX_0 (Y_D_0 * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 0)]) * m - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 0)] * delta) - ref_IS_M1 * (Y_D_0 / Y_D_sum) * w_GE
   set CSX_1 (Y_D_1 * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 1)]) * m - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 1)] * delta) - ref_IS_M1 * (Y_D_1 / Y_D_sum) * w_GE
   set CSX_2 (Y_D_2 * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 2)]) * m - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 2)] * delta) - ref_IS_M1 * (Y_D_2 / Y_D_sum) * w_GE
   ]
   
end
@#$#@#$#@
GRAPHICS-WINDOW
235
121
670
467
15
11
13.7143
1
10
1
1
1
0
0
0
1
-15
15
-11
11
0
0
1
ticks
30.0

BUTTON
24
22
87
55
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
112
23
175
56
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
13
156
187
189
x_s_max
x_s_max
1
100
40.6481
1
1
NIL
HORIZONTAL

SLIDER
13
194
188
227
h_max
h_max
1
100
5.719
1
1
NIL
HORIZONTAL

SLIDER
14
670
174
703
l
l
0
1
2.2267
0.1
1
NIL
HORIZONTAL

SLIDER
10
258
182
291
Y_D_0
Y_D_0
1
100
50
1
1
NIL
HORIZONTAL

SLIDER
185
544
325
577
beta
beta
0.01
1
0.4559
0.01
1
NIL
HORIZONTAL

SLIDER
187
586
327
619
theta
theta
0
2
0.0578
0.05
1
NIL
HORIZONTAL

PLOT
813
164
1154
322
Energy supply and demand
time 
energy
0.0
10.0
0.0
160.0
true
true
"" ""
PENS
"Y_D" 1.0 0 -16777216 true "" "plot sum [y_d_j] of cities"
"Y_S_IS" 1.0 0 -16777216 true "" "plot y_s_IS"
"y_s_0" 1.0 0 -2674135 true "" "plot y_s_0"
"y_s_1" 1.0 0 -13345367 true "" "plot y_s_1"
"Y_s_2" 1.0 0 -13840069 true "" "plot y_s_2"
"Y_D_0" 1.5 2 -2674135 true "" "plot Y_D_0"
"Y_D_1" 1.5 2 -13345367 true "" "plot Y_D_1"
"Y_D_2" 1.5 2 -13840069 true "" "plot Y_D_2"

TEXTBOX
795
10
1052
29
Model-Output
15
0.0
1

TEXTBOX
15
130
165
148
Landscape Properties
14
0.0
1

TEXTBOX
12
236
162
254
Consumer Properties 
14
0.0
1

TEXTBOX
186
480
336
498
External Cost Parameter
14
0.0
1

SLIDER
185
505
326
538
alfa
alfa
0
100
113.0454
0.01
1
NIL
HORIZONTAL

SLIDER
12
588
173
621
w_GC
w_GC
0
1
0.066428
0.05
1
NIL
HORIZONTAL

SLIDER
12
505
174
538
p
p
0
2
1
0.05
1
NIL
HORIZONTAL

SLIDER
12
547
173
580
w_PC
w_PC
0
30
3.3214
1
1
NIL
HORIZONTAL

BUTTON
15
76
95
109
NIL
setup-turtles
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
813
327
1154
454
Welfare equation elements
time
Monetary Unit
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"W" 1.0 0 -2674135 true "" "plot sum[phi_IS_MS1]  of patches with [(prod_IS = 1)] - ref_IS_M2 * w_GC + (Y_D_sum * ((p_max - p)/ 2)) + (sum[y] of patches with [(prod_IS = 1)]) * m - ref_IS_M1 * w_GE - sum[x_IS]  of patches with [(prod_IS = 1)] * delta"
"0" 1.0 0 -16777216 true "" "plot 0"
"X" 1.0 0 -11085214 true "" "plot sum[x_IS]  of patches with [(prod_IS = 1)] * delta"
"PS" 1.0 0 -6917194 true "" "plot sum[phi_IS_MS1]  of patches with [(prod_IS = 1)] - ref_IS_M2 * w_GE"
"CS" 1.0 0 -5516827 true "" "plot (Y_D_sum * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1)]) * m - ref_IS_M1 * w_GE"
"C_GE" 1.0 0 -408670 true "" "plot ref_IS_M1 * w_GE + ref_IS_M2 * w_GE"

SLIDER
186
625
327
658
delta
delta
0
10
0.0598
0.05
1
NIL
HORIZONTAL

SLIDER
13
629
172
662
w_GE
w_GE
0
1
0.0494
1
1
NIL
HORIZONTAL

TEXTBOX
15
461
165
495
Business Cost Parameter
14
0.0
1

SLIDER
9
374
181
407
p_max
p_max
0
2
1.6229
0.05
1
NIL
HORIZONTAL

SLIDER
10
412
182
445
m
m
0
1
0.2834
0.05
1
NIL
HORIZONTAL

MONITOR
880
42
952
87
PS_IS
sum[phi_IS_MS1]  of patches with [(prod_IS = 1)] - ref_IS_M2 * w_GE
2
1
11

MONITOR
964
42
1039
87
CS_IS
(Y_D_sum * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1)]) * m - ref_IS_M1 * w_GE
2
1
11

MONITOR
1054
42
1125
87
X_IS
sum[x_IS]  of patches with [(prod_IS = 1)] * delta
2
1
11

MONITOR
794
42
868
87
W_IS
sum[phi_IS_MS1]  of patches with [(prod_IS = 1)] - ref_IS_M2 * w_GE\n+ (Y_D_sum * ((p_max - p)/ 2))\n+ (sum[y]  of patches with [(prod_IS = 1)]) * m - ref_IS_M1 * w_GE\n- sum[x_IS]  of patches with [(prod_IS = 1)] * delta
2
1
11

MONITOR
794
103
879
148
CSX_IS_0
(Y_D_0 * ((p_max - p)/ 2)) \n+ (sum[y]  of patches with [(prod_IS = 1) and (city_id = 0)]) * m \n- (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 0)] * delta) \n- ref_IS_M1 * (Y_D_0 / Y_D_sum) * w_GE
2
1
11

MONITOR
890
102
975
147
CSX_IS_1
(Y_D_1 * ((p_max - p)/ 2))\n+ (sum[y]  of patches with [(prod_IS = 1) and (city_id = 1)]) * m\n-(sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 1)] * delta)\n- ref_IS_M1 * (Y_D_1 / Y_D_sum) * w_GE
2
1
11

MONITOR
985
102
1070
147
CSX_IS_2
(Y_D_2 * ((p_max - p)/ 2))\n+ (sum[y]  of patches with [(prod_IS = 1) and (city_id = 2)]) * m\n-(sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 2)] * delta)\n- ref_IS_M1 * (Y_D_2 / Y_D_sum) * w_GE
2
1
11

SLIDER
10
297
182
330
Y_D_1
Y_D_1
1
100
50
1
1
NIL
HORIZONTAL

SLIDER
10
335
182
368
Y_D_2
Y_D_2
0
100
50
1
1
NIL
HORIZONTAL

MONITOR
1135
42
1200
87
C_GE_IS
ref_IS_M1 * w_GE + ref_IS_M2 * w_GE
2
1
11

PLOT
812
461
1154
589
Regional utility per consumer centre
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"City 1" 1.0 0 -3844592 true "" "plot (Y_D_0 * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 0)]) * m - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 0)] * delta) - ref_IS_M1 * (Y_D_0 / Y_D_sum) * w_GE"
"City 2" 1.0 0 -14070903 true "" "plot (Y_D_1 * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 1)]) * m - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 1)] * delta) - ref_IS_M1 * (Y_D_1 / Y_D_sum) * w_GE"
"City 3" 1.0 0 -10899396 true "" "plot (Y_D_2 * ((p_max - p)/ 2)) + (sum[y]  of patches with [(prod_IS = 1) and (city_id = 2)]) * m - (sum[x_IS]  of patches with [(prod_IS = 1) and (city_id = 2)] * delta) - ref_IS_M1 * (Y_D_2 / Y_D_sum) * w_GE"
"0" 1.0 0 -16777216 true "" "plot 0"

SWITCH
194
63
284
96
MS2
MS2
1
1
-1000

MONITOR
731
634
787
679
Producer
sum[prod_IS]  of patches with [(prod_IS = 1)]
17
1
11

MONITOR
731
577
788
622
C_inv
sum[prod_IS]  of patches with [(prod_IS = 1)] * W_PC * l
2
1
11

MONITOR
729
521
786
566
C_con
sum[d_iv_is]  of patches with [(prod_IS = 1)] * W_GC
2
1
11

MONITOR
662
636
719
681
min h
min[h] of patches
2
1
11

MONITOR
663
521
720
566
max h
max [h] of patches
2
1
11

MONITOR
690
251
761
296
mean h 2  
mean [h] of patches with [(city_ID = 2)]
2
1
11

MONITOR
690
136
761
181
mean h 0  
mean [h] of patches with [(city_ID = 0)]
2
1
11

MONITOR
689
191
760
236
mean h 1  
mean [h] of patches with [(city_ID = 1)]
2
1
11

MONITOR
662
578
719
623
mean h
mean [h] of patches
2
1
11

SLIDER
344
508
478
541
grid_ratio
grid_ratio
0
1
0.8068
0.01
1
NIL
HORIZONTAL

SWITCH
294
63
397
96
MS3
MS3
1
1
-1000

TEXTBOX
199
27
436
61
Market-based Instruments
14
0.0
1

SLIDER
343
549
479
582
psi
psi
-1
0
-0.2
0.05
1
NIL
HORIZONTAL

SLIDER
342
588
480
621
omega
omega
0
10
1.7783889901823224
0.1
1
4
HORIZONTAL

SWITCH
407
63
510
96
MS3.1
MS3.1
0
1
-1000

SWITCH
536
62
626
95
CO
CO
0
1
-1000

TEXTBOX
537
30
687
48
Coalition
14
0.0
1

TEXTBOX
348
479
498
497
Policy Parameter\n
14
0.0
1

SWITCH
685
38
775
71
EX
EX
1
1
-1000

TEXTBOX
500
477
650
495
Coalition Parameter
14
0.0
1

SLIDER
494
507
631
540
lambda
lambda
0
1
0.5859
0.05
1
NIL
HORIZONTAL

MONITOR
690
311
770
356
mean x_s_0
mean [x_s] of patches with [(city_ID = 0)]
2
1
11

MONITOR
691
373
764
418
mean x_s_1
mean [x_s] of patches with [(city_ID = 1)]
2
1
11

MONITOR
691
433
765
478
mean x_s_2
mean [x_s] of patches with [(city_ID = 2)]
2
1
11

INPUTBOX
631
38
681
98
PA
0
1
0
Number

PLOT
816
595
1157
722
Regional welfare per consumer centre
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"0" 1.0 0 -16777216 true "" "plot 0"
"City 1" 1.0 0 -2674135 true "" "plot W_0"
"City 2" 1.0 0 -14070903 true "" "plot W_1"
"City 3" 1.0 0 -14439633 true "" "plot W_2"

SLIDER
342
626
480
659
s
s
0
0.5
0.432
1
1
NIL
HORIZONTAL

SWITCH
686
80
776
113
SU
SU
0
1
-1000

MONITOR
547
592
597
637
s1
s0
2
1
11

MONITOR
493
592
543
637
s0
s0
4
1
11

MONITOR
601
592
651
637
s2
s2
4
1
11

SLIDER
494
548
632
581
gamma
gamma
0
2
0.9157
0.1
1
NIL
HORIZONTAL

SLIDER
342
668
482
701
t
t
0
1
0.432
0.012
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
false
0
Polygon -7500403 true true 150 30 135 30 135 150 150 150
Polygon -7500403 true true 135 45 105 75 105 60 135 30 90 0 105 0 150 30
Polygon -7500403 true true 150 30 195 45 195 60 135 30

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

instrument
false
0
Polygon -7500403 true true 150 30 165 30 165 150 150 150
Polygon -7500403 true true 165 45 195 75 195 60 165 30 210 0 195 0 150 30
Polygon -7500403 true true 150 30 105 45 105 60 165 30

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

optimal
false
0
Polygon -7500403 true true 150 30 135 30 135 150 150 150
Polygon -7500403 true true 135 45 105 75 105 60 135 30 90 0 105 0 150 30
Polygon -7500403 true true 150 30 195 45 195 60 135 30

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-turtles</setup>
    <go>go</go>
    <metric>depth-gauge</metric>
    <metric>tot_c_1</metric>
    <metric>tot_c_2</metric>
    <enumeratedValueSet variable="beta">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_n">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFRA-Intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_location_cost">
      <value value="43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="performance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alfa">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_energy_potential">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_e">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy_demand">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_l">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
