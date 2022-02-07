#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode};
use cumulus_primitives_core::{relay_chain, ParaId, ServiceQuality, XcmpMessageHandler};
use frame_support::traits::OnKilledAccount;
use frame_support::{
	dispatch::DispatchResult,
	sp_runtime::traits::Hash,
	sp_runtime::RuntimeDebug,
	traits::{BalanceStatus::Free, Currency, Get, ReservableCurrency},
};
pub use pallet::*;
use pallet_common::{DeviceState, Order, OrderBase, XCMPMessage};
use sp_std::convert::{TryFrom, TryInto};
use sp_std::prelude::*;
use xcm::v0::{Junction, OriginKind, SendXcm, Xcm};
use xcm::VersionedXcm;

type XCMPMessageOf<T> = XCMPMessage<
	<T as frame_system::Config>::AccountId,
	BalanceOf<T>,
	<T as Config>::OrderPayload,
	<T as pallet_timestamp::Config>::Moment,
>;

pub(crate) type OrderBaseOf<T> = OrderBase<
	<T as Config>::OrderPayload,
	BalanceOf<T>,
	MomentOf<T>,
	<T as frame_system::Config>::AccountId,
>;

pub(crate) type OrderOf<T> = Order<
	<T as Config>::OrderPayload,
	BalanceOf<T>,
	MomentOf<T>,
	<T as frame_system::Config>::AccountId,
	ParaId,
>;

pub type BalanceOf<T> =
	<<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;
pub type MomentOf<T> = <T as pallet_timestamp::Config>::Moment;
type Timestamp<T> = pallet_timestamp::Pallet<T>;

#[frame_support::pallet]
pub mod pallet {
	use super::{
		BalanceOf, DeviceState, Junction, MomentOf, OrderBaseOf, OrderOf, OriginKind, ParaId,
		ReservableCurrency, SendXcm, ServiceQuality, Timestamp, Xcm,
	};
	use frame_support::pallet_prelude::*;
	use frame_support::{
		sp_runtime::traits::Hash,
		traits::{tokens::ExistenceRequirement, Currency, Randomness},
		transactional,
	};
	use frame_system::pallet_prelude::*;
	use scale_info::TypeInfo;
	use sp_io::hashing::blake2_128;

	#[cfg(feature = "std")]
	use frame_support::serde::{Deserialize, Serialize};

	type AccountOf<T> = <T as frame_system::Config>::AccountId;

	// Struct for holding Kitty information.
	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	pub struct Kitty<T: Config> {
		pub duration: Option<MomentOf<T>>,
		pub penalty: Option<BalanceOf<T>>,
		pub state: DeviceState,
		pub paraid: ParaId,
	}
	// Enum declaration for Gender.
	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
	pub enum Gender {
		Male,
		Female,
	}

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	/// Configure the pallet by specifying the parameters and types it depends on.
	#[pallet::config]
	pub trait Config: frame_system::Config + pallet_timestamp::Config {
		/// Because this pallet emits events, it depends on the runtime's definition of an event.
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
		type OrderPayload: Encode + Decode + Clone + Default + Parameter;
		type Currency: ReservableCurrency<Self::AccountId>;

		/// The maximum amount of Kitties a single account can own.
		#[pallet::constant]
		type MaxKittyOwned: Get<u32>;

		/// The type of Randomness we want to specify for this pallet.
		type KittyRandomness: Randomness<Self::Hash, Self::BlockNumber>;
	}

	// Errors.
	#[pallet::error]
	pub enum Error<T> {
		NoneValue,
		OrderExists,
		IllegalState,
		Overdue,
		DeviceLowBail,
		DeviceExists,
		BadOrderDetails,
		NoDevice,
		NoOrder,
		Prohibited,
		CannotReachDestination,
	}

	// Events.
	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		NewDevice(T::AccountId),
		NewOrder(T::AccountId, T::AccountId),
		Accept(T::AccountId, T::AccountId),
		Reject(T::AccountId, T::AccountId),
		Done(T::AccountId, T::AccountId),
		BadVersion(<T as frame_system::Config>::Hash),
	}

	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	pub struct DeviceProfile<T: Config> {
		pub dna: [u8; 16], // Using 16 bytes to represent a kitty DNA
		pub price: Option<BalanceOf<T>>,
		pub gender: Gender,
		pub owner: AccountOf<T>,
	}
	// Storage items.

	#[pallet::storage]
	#[pallet::getter(fn orders)]
	pub type Orders<T: Config> = StorageMap<_, Twox64Concat, T::AccountId, OrderOf<T>, OptionQuery>;

	/// Device profiles
	#[pallet::storage]
	#[pallet::getter(fn devices)]
	pub type Device<T: Config> =
		StorageMap<_, Twox64Concat, T::AccountId, DeviceProfile<T>, OptionQuery>;
}
