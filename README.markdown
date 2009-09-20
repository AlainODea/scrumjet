ScrumJet: RESTful task tracking experiment built with webmachine
================================================================

ScrumJet's experimental premise is simple: I wanted to learn more about [webmachine] [1].  Basic task tracking is a fairly simple thing to implement and has lots of obvious ways to link things together.

Running the Experiment
----------------------

Pre-requisitites:

* Mercurial (to get webmachine)
* Erlang/OTP

Shell 1:
<pre>
make
./start-dev.sh
</pre>

Shell 2:
<pre>
./load-test-data.sh
</pre>

Open [http://127.0.0.1:8000/] [8] in a web browser.

Motivation
----------

REST(Representational State Transfer) is an architectural style which can be applied well to such a system and webmachine makes it easy to implement in [Erlang] [2].

Overall Architecture
--------------------

ScrumJet is broken out into _resources_ for _tasks_, _categories_, _boards_ and utility resources for representing linkages amongst them.  Much of the motivation for this architecture came from reading [O'Reilly's book RESTful Web Services] [3].  As such it focuses on providing a well-connected HTML representation of each resource.  I am likely to eventually add [JSON] [7] representations to the resources for efficiency, but my goal at the moment is a proper RESTful architecture.  I find it easier to achieve that in HTML.

Part of the motivation for this architecture is a secondary educational goal of learning how to build an ARIA on top of a RESTful web service using [Dojo] [5].  Obviously [Dojo] [5] prefers [JSON] [7] so when I get ScrumJet to that point I will certainly want the resources to be able to represent themselves as [JSON] [7].

### Underlying Architecture

ScrumJet's _resources_ are implemented as [webmachine] [1] resources.  Each resource generates an HTML representation based on state stored in one or more [mnesia] [6] tables.  Specifically, each resource uses a gen_server to work with its primary state in an mnesia table and uses the gen_servers of other resources to determine connections or links to be rendered in the representation.

[1]: http://bitbucket.org/justin/webmachine/wiki/Home
[2]: http://erlang.org/
[3]: http://oreilly.com/catalog/9780596529260/
[4]: http://json.org/
[5]: http://dojotoolkit.com/
[6]: http://erlang.org/doc/apps/mnesia/
[7]: http://json.or/
[8]: http://127.0.0.1:8000/