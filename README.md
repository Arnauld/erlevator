# Get Started

Compile the application using rebar:

    ./rebar get-deps compile

Start the application:

    erl -pa ebin deps/*/ebin -s erlevator

Execute tests:

    ./rebar compile eunit skip_deps=true

# Links

* [cowboy](https://github.com/extend/cowboy): Small, fast, modular HTTP server written in Erlang
* [Create and Deploy Your Erlang / Cowboy Application on Heroku](http://roberto-aloi.com/blog/2013/07/13/create-deploy-erlang-cowboy-application-heroku/)
*

