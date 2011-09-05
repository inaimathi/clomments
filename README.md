## Usage as a client 
###### Assuming clomments.inaimathi.ca, but you can replace that with any clomments server once more become available

Add

    <script src='https://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js'></script> 
    <script src='http://clomments.inaimathi.ca/clomments.js'></script> 
    <link rel='stylesheet' type='text/css' href='http://clomments.inaimathi.ca/clomments.css' />

to the `<head>` of your page (note that the stylesheet can be omitted if you'd like to provide your own styles).

Add

    <div id="clomments"></div>
    
somewhere in the `<body>`.

That's it.


## Usage as a server
###### Short-ish server installation for Debian:

1. `apt-get install sbcl mysql-server cl-sql`
2. `git clone https://github.com/Inaimathi/clomments.git`
3. Create a database and user, and change the definition of `*db-spec*` in `package.lisp` to match the information; *it's `'("server" "user" "database" "password")`)*
4. Register with [recaptcha](http://www.google.com/recaptcha) and fill in the `*public-key*` and `*private-key*` variables in `package.lisp` as appropriate.
5. install [quicklisp](http://www.quicklisp.org/beta/) *you can do without it, but it's more complicated 'cause you need to `asdf-install` everything more or less manually*
6. `cd clomments; sbcl --eval "(ql:quickload :clomments)"`
7. In SBCL, type 

    `(in-package :clomments)`  
    `(new-database)`

Go to `http://localhost:4242/test` in a browser.

To function as a public server, you need to register a domain and point it to your server (don't do that until this actually becomes somewhat useful and stable).
