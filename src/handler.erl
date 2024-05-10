-module(handler).
-export([handle/1, parse/1, route/1, format_response/1]).

% """
% GET /wildthings HTTP/1.1
% Host: example.com
% User-Agent: ExampleBrowser/1.6
% Accept: */*

% """
% 
% Request = 
% "GET /wildthings HTTP/1.\n"
% "Host: example.com\n"
% "User-Agent: ExampleBrowser/1.6\n"
% "Accept: */*\n\n", 


handle(Request) ->
    Map1 = parse(Request),
    Map2 = route(Map1),
    format_response(Map2).

parse(Request) ->
    [First, _Second, _Third | _T] = string:tokens(Request, "\n"),
    [Method, Path, _Version] = string:tokens(First, " "),
    #{method => Method, path => Path, status => undefined, resp_body => ""}.


route(#{method := "GET", path := "/wildthings"} = Map) ->
    Map#{status := 200, resp_body := "Bears, Lions, Tigers"};
route(#{method := "GET", path := "/bears"} = Map) ->
    Map#{status := 200, resp_body := "Bigfoot, Teddy"};
route(#{method := _Method, path := _Path} = Map) ->
    Map#{status := 404, resp_body := "Error: Not found"}.

format_response(Map) ->
    % TODO: Uses value in map to create an HTTP response string
    % 
    % #{method => "GET",path => "/wildthings", resp_body => "Bears, Lions, Tigers"}
    
    % """
    % HTTP/1.1 200 OK
    % Content-Type: text/html
    % Content-Length: 13
    % 
    % Bears, Lions, Tigers
    % """
    #{status := Status, resp_body := Body} =  Map,
    Length = string:length(Body),
    Reason = status_reason(Status),

    
    "HTTP/1.1 " ++ integer_to_list(Status) ++ " " ++ Reason ++ "\n" ++
    "Content-Type: text/html\n"
    "Content-Length: " ++ integer_to_list(Length) ++"\n\n"
    ++ Body ++ "\n".


status_reason(Status) ->
    case Status of
        200 -> "OK";
        201 -> "Created";
        404 -> "Not found";
        401 -> "Unauthorized";
        403 -> "Forbidden";
        500 -> "Internal Server Error"
    end.
