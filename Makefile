all: clean
	cd ./data_gen && cargo build --release && cp ./target/release/data_gen ./out_data_gen && cd ..

clean:
	rm -f ./out_data_gen; \
