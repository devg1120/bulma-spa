
----------------------------
# elm reactor

## files

 - index.html

## make & hot

 - elm reactor
 - http://127.0.0.1:8000/

----------------------------
# httpd

## files

 - index.html

## make & httpd

 - ./build.sh (make)

   ```
   elm make src/Main.elm --output elm.js
   ```

 - ./Lighttpd_start.sh  (httpd start)


 - http://127.0.0.1:8000/

----------------------------
# parcel 

## files

 - index_parcel.html
 - js/index.js

## parcel start (debug)

 - npm start

    ```
    parcel index_parcel.html --out-dir dist
    ```
 - http://127.0.0.1:1234/ 

## parcel build (production)

 - npm build

    ```
    parcel build index_parcel.html --out-dir dist
    ```
 - output 
    ```
          ./dist
                 index.html
                 ****.js
    ```

