structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/newapi/v1/landmarks", 
    status_code = 201L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Mon, 06 Feb 2023 14:56:34 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Transfer-Encoding` = "chunked", Connection = "close", 
        Location = "/landmarks?id=eq.23012", `Content-Range` = "*/*", 
        `Content-Profile` = "public"), class = "httr2_headers"), 
    body = charToRaw("[{\"id\":23012,\"campaign_id\":15497,\"tree_code\":null,\"taxa_name\":\"Malus\",\"dbh\":850,\"dbh_unit\":null,\"azimut\":95,\"distance\":null,\"distance_unit\":null,\"geom\":{\"type\":\"Point\",\"coordinates\":[-79.427055,48.4367]},\"type\":\"gps\",\"notes\":null,\"created_at\":\"2023-02-06T09:56:34.124076-05:00\",\"updated_at\":\"2023-02-06T09:56:34.124076-05:00\",\"trap_id\":null,\"device_id\":null,\"lure_id\":null,\"axis\":null,\"thermograph_type\":null}]")), class = "httr2_response")
