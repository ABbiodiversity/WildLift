# Version 0.2.4

* Fixes to the in app:
  - sliders rounded demogr rates to 2 digits but some were pre set to 3 digits, now using 0.001 steps
  - percent was used instead of proportion in breakeven calculation that gave error, fixed
  - predator tab used mat pen defaults when before demogr sliders were rendered, fixed

# Version 0.2.3

* Added `COMPLIANCE.yaml` and updated `.Rbuildignore`.

# Version 0.2.2

* Added transportation related mortality to captive breeding.

# Version 0.2.1

* Added captive breeding component (#5).
* Objects now store call for easier updating.

# Version 0.2.0

* User interface for wolf reduction is finalized.
* File download issue fixed.

# Version 0.1.1

* `caribou_forecast` gained new argument `fpen.inds` to specify
  number of penned females, can be a vector representing subsequent years 
  ([#3](https://github.com/bcgov/CaribouBC/issues/3)).
* Three new herds added to maternity penning, and also for a new 
  treatment type for wolf reduction (argument `pen.type = "wolf.red"`).
  ([#4](https://github.com/bcgov/CaribouBC/issues/4)).

# Version 0.1.0

* 1st production release.

# Version 0.0.2

* Modified Moose reduction tab.

# Version 0.0.1

* Added initial functionality to the package.
