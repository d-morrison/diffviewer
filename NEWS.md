# diffviewer (development version)

* Added support for comparing `.rds` files containing R objects using the waldo package.
  - Data frames in `.rds` files are compared using daff.js
  - Other R objects (excluding plots) are compared using waldo
  - Plot objects are displayed as text

# diffviewer 0.1.2

* diffviewer gets a sizing policy that works better in Rmds, and some css tweaks to ensure it's readable in dark mode.

# diffviewer 0.1.1

* The diffviewer widget previously ignored some very minor pixel differences, 
  but now it will show every difference (#9, #11).

# diffviewer 0.1.0

* Added a `NEWS.md` file to track changes to the package.
