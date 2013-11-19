# Get Started

Compile the application using rebar:

    ./rebar get-deps compile

Start the application:

    erl -pa ebin deps/*/ebin -s erlevator

Execute tests:

    ./rebar compile eunit skip_deps=true

Execute tests for dependencies too:

    ./rebar compile eunit


# Heroku

Deploy on Heroku

    git push heroku master


Check logs

    heroku logs

Remote bash

    heroku run bash


# Live?

    http://elevator-scores.ybonnel.cloudbees.net/


    http://elevator.retour1024.eu.cloudbees.net


# Links

* [cowboy](https://github.com/extend/cowboy): Small, fast, modular HTTP server written in Erlang
* [Create and Deploy Your Erlang / Cowboy Application on Heroku](http://roberto-aloi.com/blog/2013/07/13/create-deploy-erlang-cowboy-application-heroku/)
*

