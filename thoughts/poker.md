type Rank = A; Tw; Th; Fo; Fi; Si; Se; E; N; T; J; Q; K
type Suit = Hearts; Spades; Diamonds; Clubs
type Card Rank Suit
type Player
  hand: Array Card
  name: Str
  money: Ref Float
type Game 
  players: Array Player
  current_players: Array Player
  dealer: Ref Player
  pot: Ref Float
  current_bet: Ref Float
  cards_on_table: Array Card

with self: Game
  self.add_player! p = self.players.append! p
  self.play_round = self.current_players after
    self.deal
    last_who_raised = ref Nothing
    self.players.cycle $ p ->
      if not last_who_raised then last_who_raised := Just p
      else if last_who_raised == Just p then break
      case p.bet_or_fold self of
        Fold -> self.current_players.remove! p
        Bet amount ->
          if amount > !(self.current_bet)
            last_who_raised := Just p
          else if amount < !(self.current_bet)
            throw Exception('Bet is too low')
          pot += n

