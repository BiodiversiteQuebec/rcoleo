structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/newapi/v1/landmarks", 
    status_code = 201L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Thu, 08 Jun 2023 15:04:46 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Transfer-Encoding` = "chunked", Connection = "keep-alive", 
        Location = "/landmarks?id=eq.38090", `Content-Range` = "*/*", 
        `Content-Profile` = "public"), class = "httr2_headers"), 
    body = charToRaw("[{\"id\":38090,\"campaign_id\":29699,\"tree_code\":null,\"taxa_name\":null,\"dbh\":null,\"dbh_unit\":null,\"azimut\":null,\"distance\":null,\"distance_unit\":null,\"geom\":{\"type\":\"Point\",\"coordinates\":[-73.81944,45.00642]},\"type\":\"gps\",\"notes\":null,\"created_at\":\"2023-06-08T11:04:46.582182-04:00\",\"updated_at\":\"2023-06-08T11:04:46.582182-04:00\",\"trap_id\":null,\"device_id\":20585,\"lure_id\":null,\"axis\":null,\"thermograph_type\":null}]")), class = "httr2_response")
