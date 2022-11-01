use apca::data::v2::stream::drive;
use apca::data::v2::stream::MarketData;
use apca::data::v2::stream::RealtimeData;
use apca::data::v2::stream::IEX;
use apca::ApiInfo;
use apca::Client;
use apca::Error;
use clap::Parser;
use futures::FutureExt as _;
use futures::StreamExt as _;
use futures::TryStreamExt as _;
use tokio;

static DEFAULT_NHISTORY: usize = 50; // number of points captured per stream
static DEFAULT_DATAPOINTS: usize = 1; // number of streams captured per run
static DEFAULT_TDELAY: u64 = 60; // delay between streams per second

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    symbols: String,

    #[arg(short, long, default_value_t = DEFAULT_DATAPOINTS)]
    datapoints: usize,

    #[arg(short, long, default_value_t = DEFAULT_NHISTORY)]
    nhistory: usize,

    #[arg(short, long, default_value_t = DEFAULT_TDELAY)]
    tdelay: u64,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let mut data = MarketData::default();
    let mut delay = tokio::time::interval(std::time::Duration::from_secs(args.tdelay));

    data.set_bars(["SPY", "XLK"]);
    //data.set_quotes(args.symbols.split(" ").collect());
    data.set_quotes(["AAPL"]);

    let mut i = 0;
    let mut _stream_res: Vec<Vec<String>> = Vec::with_capacity(args.datapoints);
    while i < args.datapoints {
        let (mut stream, mut subscription) = client.subscribe::<RealtimeData<IEX>>().await.unwrap();
        let subscribe = subscription.subscribe(&data).boxed();
        delay.tick().await;
        let () = drive(subscribe, &mut stream)
            .await
            .unwrap()
            .unwrap()
            .unwrap();

        let () = stream
            .take(args.nhistory)
            .map_err(Error::WebSocket)
            .try_for_each(|result| async {
                result
                    .map(|data| println!("{:?}", data))
                    .map_err(Error::Json)
            })
            .await
            .unwrap();
        i = i + 1;
    }

    // TODO: write data to csv file
}
