#' Calculate spatial average from pacea objects and defined by these grids;
#' default area is roughly whole coast
#'
#' @param pacea_st_obj
#' @param area Provide a geometry object or defaults to the whole BC coast
#'
#' @return
#' @export
#'
#' @examples
spatial_average <- function(pacea_st_obj,
                            area = list(matrix(c(-134, -131, -128, -124, -125.5, -134, -134,
                                                 54.4, 54.4, 50.5, 48, 47.5, 52, 54.4),
                                               ncol = 2))
){
  stopifnot("pacea_st_obj must be of class pacea_st" =
              ("pacea_st" %in% class(pacea_st_obj)))

  if(is(area, "sfc_POLYGON")) {
    area_sf <- area |> sf::st_transform(crs = 3005)
  } else {
    # convert area to a simple features object, with the correct projection
    area_sf <- sf::st_sfc(sf::st_polygon(area),
                          crs = 4326) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = 3005)
  }
  # this filters to just the required area
  obj_area <- pacea_st_obj[area_sf, ]

  obj_area_drop <- sf::st_drop_geometry(obj_area) %>%
    as_tibble()

  avg <- colMeans(obj_area_drop)

  obj_area_tib <- tibble::tibble(value = avg)

  obj_area_tib$year <- as.numeric(substr(names(avg),
                                         1,
                                         4))
  obj_area_tib$month <- as.numeric(substr(names(avg),
                                          6,
                                          7))

  obj_area_tib <- dplyr::relocate(obj_area_tib,
                                  year,
                                  month)
  obj_area_tib
}



#' Extract spatial and temporal averaged environmental variables
#'
#' @param variable Annual index (column named anamoly or value) or spatial layer from pacea
#' @param grid Grid with sf geometry
#' @param months Vector of numeric months to average
#' @param description Label for output variable in quotations
#'
#' @export
#'
#' @examples
extract_enviro_var <- function(variable, description, months = c(1,2,3,4,5,6,7,8,9,10,11,12), grid = NULL){

  if("pacea_st" %in% class(variable)){
    spvar <- spatial_average(variable, area = grid$geometry)
  } else {
    spvar <- variable
  }

  if("anomaly" %in% names(spvar)){
    spvar <- spvar |> rename(value = anomaly)
  }

  spvar |> filter_months(months, description)

}
