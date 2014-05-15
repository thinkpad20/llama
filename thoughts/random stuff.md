type ConfigItemId = Str
object EmbeddedConfigItemType (schemaVersion: Str = "",
                               broken: Bool = False,
                               errors: [{Str => Str}] = [])
object NamedType (name: Str)
object Argument (name: Str) (type: ConfigItemId)
object ArgumentBinding (name: Str) (value: Scrimshaw)
object AnalyticCluster (produces: [Argument]) 
                       (derived_values: [ArgumentBinding])

object GoalType =
  FeatureOverTimeGoal (
    content_type: ConfigItemId,
    when_up: Maybe ConfigItemId,
    when_down: Maybe ConfigItemId,
    when_same: Maybe ConfigItemId,
    name: Str = '',
    smart_context: [SmartContextBinding] = [],
    arguments: [Argument] = [],
    feature_key: Str = '',
    time_delta: Int = 90,
    epsilon: Float = 1,
    human_feature_key: Str = '',
    human_time_delta: Str = "ninety days",
    detail: Int = 0,
    tone: Str = "professional"
    )
  CohortComparison
  MetricDriver
  with childrenContent: [ConfigItemId] = []

foo : a -> b -> c
foo x = y => 2 x + y ^ 2
alias foo <+*>

assert 10 <*+> 12 == 164

object Maybe a = Nothing; Just a

trait Container c with lift : a -> c a

implement Container for Maybe with lift = Just

trait Functor f with map : (a -> b) -> f a -> f b

implement Functor for Maybe with 
  map f = Nothing => Nothing
        | (Just a) => Just $ f a

trait Applicative f with 
  Container f
  Functor f
  apply: f (a -> b) -> f a -> f b
  alias apply: <*>, left associative, precedence 3

implement Applicative for Maybe with
  apply = Nothing => _ => Nothing
        | Just f => Just a => Just (f a)
                 | Nothing => Nothing

trait Monad m with
  Applicative m
  bind: m a -> (a -> m b) -> m b
  alias bind: >>=, left associative, precedence 1

implement Monad for Maybe with
  bind ma f = case ma of
    Nothing => Nothing
    Just a  => f a 

