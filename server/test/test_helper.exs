ExUnit.start

Mix.Task.run "ecto.create", ~w(-r Snake.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Snake.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(Snake.Repo)

