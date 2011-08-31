## Usage as a client 
###### assuming `clomments.inaimathi.ca`, but yo can replace that with any `clomments` server once more become available

Add

    <script src='https://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js'></script> 
    <link rel='stylesheet' type='text/css' href='http://clomments.inaimathi.ca/clomments.css' />
    <script src='http://clomments.inaimathi.ca/clomments.js'></script> 

to the `<head>` of your page and

    <div id="clomments"></div>
    
somewhere in the `<body>`.

That's it.


## Usage as a server
###### Short-ish server installation for Debian:

1. `apt-get install sbcl mysql-server cl-sql`
2. `git clone https://github.com/Inaimathi/clomments.git`
3. Create a database and user, and change the definition of `*db-spec*` in `package.lisp` to match the information; *it's `'("server" "user" "database" "password")`)*
4. install [quicklisp](http://www.quicklisp.org/beta/) *you can do without it, but it's more complicated 'cause you need to `asdf-install` everything more or less manually*
5. `cd clomments; sbcl --eval "(ql:quickload :clomments)"`
6. In SBCL, type 

        (create-view-from-class 'comment)  
        (create-view-from-class 'page)

Go to `http://localhost:4242/test` in a browser.

To function as a public server, you need to register a domain and point it to your server (don't do that until this actually becomes somewhat useful and stable).
