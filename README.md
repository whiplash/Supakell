```
   _____                   _        _ _ 
  / ____|                 | |      | | |
 | (___  _   _ _ __   __ _| | _____| | |
  \___ \| | | | '_ \ / _` | |/ / _ \ | |
  ____) | |_| | |_) | (_| |   <  __/ | |
 |_____/ \__,_| .__/ \__,_|_|\_\___|_|_|
              | |                       
              |_|                       
```

humble attempt for a Haskell Supabase client.

WIP

## Setup

1. Copy `.env.example` to `.env`:
   ```bash
   cp .env.example .env
   ```

2. Fill in your Supabase credentials in `.env`:
   ```
   SUPABASE_URL=https://your-project.supabase.co
   SUPABASE_ANON_KEY=your-anon-key-here
   ```

## Usage

### Using environment variables (recommended):

```haskell
runExample = do
  result <- createClientFromEnv
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right client -> runSupakell client $ do
      rows <- selectFrom "characters"
      liftIO $ print rows
```

### Using hardcoded credentials:

```haskell
runExample = do
  let client = createClient "https://xyzcompany.supabase.co" "anon-key"
  runSupakell client $ do
    rows <- selectFrom "characters"
    liftIO $ print rows
```
