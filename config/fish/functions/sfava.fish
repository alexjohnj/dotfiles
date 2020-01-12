function sfava -d "Start fava and open it in a browser"
    fava --port 5000 --host localhost &
    sleep 1
    open "http://localhost:5000"
    fg
end
