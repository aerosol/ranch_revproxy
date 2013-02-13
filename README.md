# ranch_proxy


Simple TCP routing proxy based on @benoitc's cowboy_proxy.
This one uses ranch 0.6.1 underneath.

Expect more stuff soon, for now it's just a quick outline.

## Example usage:


    application:start(ranch).
    ranch_proxy:serve(100, ranch_tcp, [{port, 5555}], [{proxy, {example_module, proxy_function}}]).

### See also:

    * [cowboy_revproxy](https://github.com/benoitc/cowboy_revproxy)
    * [ranch](https://github.com/extend/ranch)
