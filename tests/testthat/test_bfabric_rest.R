#R

context("bfabricShiny")

test_that("test read", {
  expect_error(bfabricShiny::read(endpoint='user',
                                    query=list(login='cpanse')))
  
  Rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
  expect_true(file.exists(Rprofile))
  source(Rprofile)
  
  expect_length(login, 1)
  expect_length(webservicepassword, 1)
  expect_true(nchar(webservicepassword)== 32)
  
  # "Ensure you have REST service running on localhost:5000"
  
  user <- bfabricShiny::read(endpoint='user',
                            query = list(login = 'cpanse'),
                            login = login,
                            webservicepassword = webservicepassword)
  
  expect_equal(user[[1]][[1]]$login , "cpanse")
  
  resources <- bfabricShiny:::getResources(login, webservicepassword,
                                           workunitid = 187604) |>
    sapply(function(x)x[['_id']])
  
  
  
  expect_true(bfabricShiny::read(endpoint = 'application',
                            query = list('id' = 19),
                            login = login,
                            webservicepassword = webservicepassword)[['res']][[1]]$name == "mascot_dat")
  
  
  # will fail once someone adds a WU to p3000
  expect_true(sum(
    c(1527155, 1527154, 1527153, 1527152, 1498605, 1498595, 1498589, 1498588,
    1498433, 1498199, 1457753, 1457752, 1457751, 1441556, 1441552, 1404513,
    1404508, 1404501, 1404498, 1401796, 1401771, 1394399, 1393917, 1393914,
    1393912, 1390294, 1385497, 1385496, 1380065, 1368227, 1368221, 1366209,
    1366207, 1365967, 1365897, 1361401, 1361400, 1361235, 1360800, 1350508,
    1350506, 1350504, 1350494, 1333755, 1333754, 1333753, 1333752, 1333751,
    1333750, 1333749, 1333748, 1333747, 1333746, 1333745, 1333699, 1333697,
    1333696, 1333579, 1333567, 1333564, 1333563, 1333384, 1333369, 1333364,
    1333358, 1333353, 1332668, 1332605, 1329380, 1329118, 1329110, 1326614,
    1326611, 1326607, 1322571, 1321363, 1320383, 1304484, 1299985, 1299984,
    1299983, 1297159, 1297157, 1284306, 1284209, 1284193, 1284190, 1284182,
    1283003, 1282997, 1282995, 1282871, 1282799, 1282798, 1282797, 1282796,
    1282795, 1279978, 1279940, 1279828) %in% resources) == 100)
  
  
  expect_true("202584 - MaxQuant_Abird_2ng" %in%
                bfabricShiny:::getWorkunits(login, webservicepassword, 3000))
  
  expect_false("202584 - MaxQuant_Abird_2ng" %in%
                 bfabricShiny:::getWorkunits(login, webservicepassword, 3001))
  
  
  expect_true(length(bfabricShiny:::getApplications(login, webservicepassword) |>
                       sapply(function(x)x[['_id']])) > 99)
  
  expect_error(.createWorkunit())
  
  
  expect_true(bfabricShiny::read(login, webservicepassword,
             endpoint = 'resource',
             query = list('filechecksum' = '127f0c5b6352a326f9a6c8458d59d921'),
  )$res[[1]][['filechecksum']] == '127f0c5b6352a326f9a6c8458d59d921')
})


  
