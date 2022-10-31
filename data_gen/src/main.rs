use apca::data::v2::stream::drive;
use apca::data::v2::stream::MarketData;
use apca::data::v2::stream::RealtimeData;
use apca::data::v2::stream::IEX;
use apca::ApiInfo;
use apca::Client;
use apca::Error;
use apca::Subscribable::Stream;
use futures::FutureExt as _;
use futures::StreamExt as _;
use futures::TryStreamExt as _;
use tokio;

static DEFAULT_HISTORY: usize = 50; // number of points captured per stream
static DEFAULT_DELAY: usize = 60; // delay between streams per second

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    symbols: String,

    #[arg(short, long, default_value_t = DEFAULT_HISTORY)]
    history: usize,

    #[arg(short, long, default_value_t = DEFAULT_DELAY)]
    delay: usize,
}

async fn stream_once(stream: Stream, history: usize) -> Vec<String> {
    let () = drive(subscribe, &mut stream)
        .await
        .unwrap()
        .unwrap()
        .unwrap();

    let mut res: Vec<String> = Vec::with_capacity(history);
    let () = stream
        .take(history)
        .map_err(Error::WebSocket)
        .try_for_each(|result| async {
            for (i, data) in result.iter().enumerate() {
                res[i] = data;
                println!("{:?}", data);
            }
            //result
            //.map(|data| println!("{:?}", data))
            //.map_err(Error::Json)
        })
        .await
        .unwrap();
    res
}

async fn stream(stream: Stream, history: usize, delay: usize) -> Vec<Vec<String>> {
    let mut delay = tokio::time::interval(std::time::Duration::from_secs(delay));
    let mut i = 0;
    let mut res: Vec<Vec<String>> = Vec::with_capacity(history);
    while i < len {
        delay.tick().await;
        res[i] = stream_once(stream, history).await;
    }
    res
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let (mut stream, mut subscription) = client.subscribe::<RealtimeData<IEX>>().await.unwrap();
    let mut data = MarketData::default();

    data.set_bars(["SPY", "XLK"]);
    data.set_quotes(args.symbols.split(" ").collect());

    let res = stream(args.history, args.delay);

    // TODO: write data to csv file
}
